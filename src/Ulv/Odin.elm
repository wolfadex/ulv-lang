module Ulv.Odin exposing (compile)

import Ulv.Parser


compile : Bool -> List Ulv.Parser.Expression -> String
compile debugMode expressions =
    """package ulv

import "core:fmt"
import "core:slice"
import "core:strings\""""
        ++ (if debugMode then
                """
import "core:log"
import "core:mem"
"""

            else
                ""
           )
        ++ """

main :: proc() {"""
        ++ (if debugMode then
                """
    when ODIN_DEBUG {
        // setup debug logging
        logger := log.create_console_logger()
        context.logger = logger

        // setup tracking allocator for making sure all memory is cleaned up
        default_allocator := context.allocator
        tracking_allocator: mem.Tracking_Allocator
        mem.tracking_allocator_init(&tracking_allocator, default_allocator)
        context.allocator = mem.tracking_allocator(&tracking_allocator)

        reset_tracking_allocator :: proc(a: ^mem.Tracking_Allocator) -> bool {
            err := false

            for _, value in a.allocation_map {
                fmt.printfln("%v: Leaked %v bytes", value.location, value.size)
                err = true
            }

            mem.tracking_allocator_clear(a)

            return err
        }

        defer reset_tracking_allocator(&tracking_allocator)
    }
"""

            else
                ""
           )
        ++ """
    env := Env{}
    internal_initialize(&env)
    defer delete(env.stack)
    defer delete(env.dict)

    // RUN PROGRAM
    for value in compiled_values {
        eval(&env, value)
    }
}

internal_initialize :: proc(env: ^Env) {
    // MATH
    append(&env.dict, Word{ label = "+", value = proc(env: ^Env) {
        right := pop(&env.stack)
        #partial switch r in right {
        case int:
            left := pop(&env.stack)
            #partial switch l in left {
            case int:
                append(&env.stack, l + r)
                return
            }
        case f64:
            left := pop(&env.stack)
            #partial switch l in left {
            case f64:
                append(&env.stack, l + r)
                return
            }
        }

        panic("Can only add ints or floats")
    }})
    append(&env.dict, Word{ label = "-", value = proc(env: ^Env) {
        right := pop(&env.stack)
        #partial switch r in right {
        case int:
            left := pop(&env.stack)
            #partial switch l in left {
            case int:
                append(&env.stack, l - r)
                return
            }
        case f64:
            left := pop(&env.stack)
            #partial switch l in left {
            case f64:
                append(&env.stack, l - r)
                return
            }
        }

        panic("Can only add ints or floats")
    }})
    append(&env.dict, Word{ label = "*", value = proc(env: ^Env) {
        right := pop(&env.stack)
        #partial switch r in right {
        case int:
            left := pop(&env.stack)
            #partial switch l in left {
            case int:
                append(&env.stack, l * r)
                return
            }
        case f64:
            left := pop(&env.stack)
            #partial switch l in left {
            case f64:
                append(&env.stack, l * r)
                return
            }
        }

        panic("Can only add ints or floats")
    }})
    append(&env.dict, Word{ label = "/", value = proc(env: ^Env) {
        right := pop(&env.stack)
        #partial switch r in right {
        case int:
            left := pop(&env.stack)
            #partial switch l in left {
            case int:
                append(&env.stack, l / r)
                return
            }
        case f64:
            left := pop(&env.stack)
            #partial switch l in left {
            case f64:
                append(&env.stack, l / r)
                return
            }
        }

        panic("Can only add ints or floats")
    }})
    // BOOLEAN LOGIC
    append(&env.dict, Word{ label = "and", value = proc(env: ^Env) {
        right := pop(&env.stack)
        #partial switch r in right {
        case bool:
            left := pop(&env.stack)
            #partial switch l in left {
            case bool:
                append(&env.stack, l && r)
                return
            }
        }

        panic("Can only 'and' booleans")
    }})
    append(&env.dict, Word{ label = "or", value = proc(env: ^Env) {
        right := pop(&env.stack)
        #partial switch r in right {
        case bool:
            left := pop(&env.stack)
            #partial switch l in left {
            case bool:
                append(&env.stack, l || r)
                return
            }
        }

        panic("Can only 'or' booleans")
    }})
    append(&env.dict, Word{ label = "not", value = proc(env: ^Env) {
        value := pop(&env.stack)
        #partial switch v in value {
        case bool:
            append(&env.stack, !v)
            return
        }

        panic("Can only 'not' booleans")
    }})
    append(&env.dict, Word{ label = "=", value = proc(env: ^Env) {
        right := pop(&env.stack)
        left := pop(&env.stack)
        append(&env.stack, internal_equal(&left, &right))
    }})
    append(&env.dict, Word{ label = "if", value = proc(env: ^Env) {
        eval(env, Name("force"))
        condition := pop(&env.stack)

        #partial switch cond in condition {
        case bool:
            when_true := pop(&env.stack)

            if cond {
                pop(&env.stack)
                append(&env.stack, when_true)
            }

            eval(env, Name("force"))
        case:
            panic("The condition of an if isn't True or False")
        }
    }})
    append(&env.dict, Word{ label = "<", value = proc(env: ^Env) {
        right := pop(&env.stack)
        left := pop(&env.stack)

        comp := internal_compare(&left, &right)

        append(&env.stack, comp == Tag("@LT"))
    }})
    append(&env.dict, Word{ label = ">", value = proc(env: ^Env) {
        right := pop(&env.stack)
        left := pop(&env.stack)

        comp := internal_compare(&left, &right)

        append(&env.stack, comp == Tag("@GT"))
    }})
    append(&env.dict, Word{ label = "<=", value = proc(env: ^Env) {
        right := pop(&env.stack)
        left := pop(&env.stack)

        comp := internal_compare(&left, &right)

        append(&env.stack, comp == Tag("@LT") || comp == Tag("@EQ"))
    }})
    append(&env.dict, Word{ label = ">=", value = proc(env: ^Env) {
        right := pop(&env.stack)
        left := pop(&env.stack)

        comp := internal_compare(&left, &right)

        append(&env.stack, comp == Tag("@GT") || comp == Tag("@EQ"))
    }})
    append(&env.dict, Word{ label = "compare", value = proc(env: ^Env) {
        right := pop(&env.stack)
        left := pop(&env.stack)

        comp := internal_compare(&left, &right)
        append(&env.stack, comp)
    }})
    // IO
    append(&env.dict, Word{ label = "print", value = proc(env: ^Env) {
        value := pop(&env.stack)
        fmt.println(value_to_print_string(value))
    }})
    append(&env.dict, Word{ label = "stack", value = proc(env: ^Env) {
        append(&env.stack, Quote(env.stack[:]))
    }})
    append(&env.dict, Word{ label = "force", value = proc(env: ^Env) {
        value := pop(&env.stack)
        #partial switch v in value {
        case Quote:
            for w in v {
                eval(env, w)
            }
        case:
            panic("Attempted to force a non-Quote")
        }
    }})
    // OPERATING ON QUOTES
    append(&env.dict, Word{ label = "map", value = proc(env: ^Env) {
        fn := pop(&env.stack)
        values := pop(&env.stack)

        #partial switch vals in values {
        case Quote:
            #partial switch f in fn {
            case Quote:
                for i := 0; i < len(vals); i += 1 {
                    local_env := Env{
                        dict = slice.clone_to_dynamic(env.dict[:], allocator = context.temp_allocator),
                    }
                    defer delete(local_env.stack)
                    defer delete(local_env.dict)
                    append(&local_env.stack, vals[i])
                    append(&local_env.stack, f)
                    eval(&local_env, Name("force"))

                    if len(local_env.stack) == 1 {
                        vals[i] = local_env.stack[0]
                    } else {
                        vals[i] = Quote(local_env.stack[:])
                    }
                }

                append(&env.stack, Quote(vals))
                return
            }

        }

        panic("Map expects 2 quotes")
    }})
    append(&env.dict, Word{ label = "fold", value = proc(env: ^Env) {
        fn := pop(&env.stack)
        result := pop(&env.stack)
        values := pop(&env.stack)

        #partial switch vals in values {
        case Quote:
            #partial switch f in fn {
            case Quote:
                for i := 0; i < len(vals); i += 1 {
                    local_env := Env{
                        dict = slice.clone_to_dynamic(env.dict[:], allocator = context.temp_allocator),
                    }
                    defer delete(local_env.stack)
                    defer delete(local_env.dict)
                    append(&local_env.stack, result)
                    append(&local_env.stack, vals[i])
                    append(&local_env.stack, f)
                    eval(&local_env, Name("force"))

                    if len(local_env.stack) == 1 {
                        result = local_env.stack[0]
                    } else {
                        result = Quote(local_env.stack[:])
                    }
                }

                append(&env.stack, result)
                return
            }

        }

        panic("Fold expects 2 quotes and an initial value")
    }})
    append(&env.dict, Word{ label = "reverse", value = proc(env: ^Env) {
        value := pop(&env.stack)

        #partial switch val in value {
        case Quote:
            rev := val[:]
            slice.reverse(rev)

            append(&env.stack, rev)
            return
        }

        panic("Reverse expects a quote")
    }})
    append(&env.dict, Word{ label = "concat", value = proc(env: ^Env) {
        values := pop(&env.stack)

        local_env := Env{
            dict = slice.clone_to_dynamic(env.dict[:], allocator = context.temp_allocator),
        }
        defer delete(local_env.stack)
        defer delete(local_env.dict)
        append(&local_env.stack, values)
        eval(&local_env, Name("force"))

        if len(local_env.stack) == 0 {
            append(&env.stack, Quote({}))
            return
        }

        #partial switch val in local_env.stack[0] {
        case string:
            internal_concat_string(env, local_env.stack[:])
        case Quote:
            internal_concat_quotes(env, local_env.stack[:])
        case:
            panic("This type cannot be concated")
        }

        return
    }})
}

internal_concat_string :: proc(env: ^Env, values: Quote) {

    local_env := Env{
        dict = slice.clone_to_dynamic(env.dict[:], allocator = context.temp_allocator),
    }
    defer delete(local_env.stack)
    defer delete(local_env.dict)
    append(&local_env.stack, values)
    eval(&local_env, Name("force"))

    strs := make([]string, len(local_env.stack))
    defer delete(strs)

    for value, i in local_env.stack {
        #partial switch val in value {
        case string:
            strs[i] = val
        case:
            panic("Expected a string for concatenation")
        }
    }

    str, err := strings.concatenate(strs[:], allocator = context.temp_allocator)

    if err != nil {
        panic("Error concating strings")
    }

    append(&env.stack, string(str))
}

internal_concat_quotes :: proc(env: ^Env, values: Quote) {
    quotes := make([]Quote, len(values))
    defer delete(quotes)

    for value, i in values {
        #partial switch val in value {
        case Quote:
            quotes[i] = val
        case:
            panic("Expected a quote for concatenation")
        }
    }

    quote, err := slice.concatenate(quotes, allocator = context.temp_allocator)

    if err != nil {
        panic("Error concating quotes")
    }

    append(&env.stack, Quote(quote))
}

internal_compare :: proc(left, right: ^Value) -> Tag {
    #partial switch r in right {
    case bool:
        panic("Booleans are equatable but not comparable")
    case int:
        #partial switch l in left {
        case int:
            if  l == r {
                return Tag("@EQ")
            } else if l < r {
                return Tag("@LT")
            } else {
                return Tag("@GT")
            }
        }
    case f64:
        #partial switch l in left {
        case f64:
            if  l == r {
                return Tag("@EQ")
            } else if l < r {
                return Tag("@LT")
            } else {
                return Tag("@GT")
            }
        }
    case string:
        #partial switch l in left {
        case string:
            if  l == r {
                return Tag("@EQ")
            } else if l < r {
                return Tag("@LT")
            } else {
                return Tag("@GT")
            }
        }
    case Name:
        panic("Names are equatable but not comparable")
    case Tag:
        panic("Tags are equatable but not comparable")
    case Quote:
        #partial switch l in left {
        case Quote:
            for i := 0; i < min(len(l), len(r)); i += 1 {
                comp := internal_compare(&l[i], &r[i])

                if comp == Tag("@LT") || comp == Tag("@GT") {
                    return comp
                }
            }

            if len(l) == len(r) {
                return Tag("@EQ")
            } else if len(l) < len(r) {
                return Tag("@LT")
            } else {
                return Tag("@GT")
            }
        }
    case Command:
        panic("Commands are equatable but not comparable")
    case Internal:
        panic("Cannot '=' internal values")
    }

    panic("Can only '>' values of the same type")
}

internal_equal :: proc(left, right: ^Value) -> bool {
    #partial switch r in right {
    case bool:
        #partial switch l in left {
        case bool:
            return l == r
        }
    case int:
        #partial switch l in left {
        case int:
            return l == r
        }
    case f64:
        #partial switch l in left {
        case f64:
            return l == r
        }
    case string:
        #partial switch l in left {
        case string:
            return l == r
        }
    case Name:
        #partial switch l in left {
        case Name:
            return l == r
        }
    case Tag:
        #partial switch l in left {
        case Tag:
            return l == r
        }
    case Quote:
        #partial switch l in left {
        case Quote:
            if len(r) != len(l) {
                return false
            }

            for i := 0; i < min(len(l), len(r)); i += 1 {
                if !internal_equal(&l[i], &r[i]) {
                    return false
                }
            }

            return true
        }
    case Command:
        #partial switch l in left {
        case Command:
            return l == r
        }
    case Internal:
        panic("Cannot '=' internal values")
    }

    panic("Can only '=' values of the same type")
}

eval :: proc(env: ^Env, value: Value) {
    switch v in value {
    case int:
        append(&env.stack, v)
    case f64:
        append(&env.stack, v)
    case bool:
        append(&env.stack, v)
    case string:
        append(&env.stack, v)
    case Quote:
        append(&env.stack, v)
    case Name:
        named_value, found := find_named(&env.dict, v)
        if found {
            #partial switch nv in named_value.value {
            case Quote:
                for w in nv {
                    eval(env, w)
                }
            case:
                eval(env, named_value.value)
            }
        } else {
            panic(fmt.aprintf("Unknown word! %s", v))
        }
    case Tag:
        append(&env.stack, v)
    case Command:
        switch v.cmd {
        case .Assign:
            named_value := pop(&env.stack)
            append(&env.dict, Word{label = v.name, value = named_value})
        case .Push:
            named_value, found := find_named(&env.dict, v.name)
            if found {
                append(&env.stack, named_value.value)
            } else {
                panic(fmt.aprintf("Unknown word! %s", v.name))
            }
        }
    case Internal:
        v(env)
    }
}

value_to_print_string :: proc(value: Value) -> (str: string) {
    switch v in value {
    case int:
        str = fmt.aprintf("%d", v, allocator = context.temp_allocator)
    case f64:
        str = fmt.aprintf("%f", v, allocator = context.temp_allocator)
    case bool:
        if v {
            str = "True"
        } else {
            str = "False"
        }
    case string:
        str = v
    case Quote:
        body_strs := make([]string, len(v))
        defer delete(body_strs)

        for val, i in v {
            body_strs[i] = value_to_print_string(val)
        }

        body_str, err := strings.join(body_strs, ", ", allocator = context.temp_allocator)

        if err != nil {
            panic("Error printing quote")
        }

        str = fmt.aprintf("(%s)", body_str, allocator = context.temp_allocator)
    case Name:
        str = string(v)
    case Tag:
        str = string(v)
    case Command:
        switch v.cmd {
        case .Assign:
            str = fmt.aprintf(":%s", v.name, allocator = context.temp_allocator)
        case .Push:
            str = fmt.aprintf("^%s", v.name, allocator = context.temp_allocator)
        }
    case Internal:
        str = "<internal>"
    }

    return
}

Env :: struct {
    stack: [dynamic]Value,
    dict: [dynamic]Word,
}

Word :: struct {
    label: Name,
    value: Value,
}

Value :: union #no_nil {int, f64, string, bool, Name, Tag, Quote, Command, Internal}

Quote :: []Value

Name :: distinct string

Tag :: distinct string

Internal :: proc(env: ^Env)

Command :: struct {
    name: Name,
    cmd: Command_Type,
}

Command_Type :: enum {
    Assign,
    Push,
}

find_named :: proc(dict: ^[dynamic]Word, label: Name) -> (w: Word, found: bool) {
    for word in dict {
        if word.label == label {
            w = word
            found = true
            return
        }
    }

    return
}

compiled_values : []Value = {
"""
        ++ compileExpressions expressions
        ++ """
}

"""


compileExpressions : List Ulv.Parser.Expression -> String
compileExpressions expressions =
    expressions
        |> List.map compileExpression
        |> String.join ", "


compileExpression : Ulv.Parser.Expression -> String
compileExpression expression =
    case expression of
        Ulv.Parser.Exp_Integer int ->
            "int(" ++ String.fromInt int ++ ")"

        Ulv.Parser.Exp_Float float ->
            "f64(" ++ String.fromFloat float ++ ")"

        Ulv.Parser.Exp_Boolean bool ->
            if bool then
                "true"

            else
                "false"

        Ulv.Parser.Exp_String string ->
            "string(\""
                ++ (string
                        -- |> Debug.log "before"
                        |> String.replace "\n" "\\n"
                        |> String.replace "\u{000D}" "\\r"
                        |> String.replace "\t" "\\t"
                        |> String.replace "\"" "\\\""
                    -- |> Debug.log "after"
                   )
                ++ "\")"

        Ulv.Parser.Exp_Name possiblyCommand (Ulv.Parser.Name name) ->
            case possiblyCommand of
                Nothing ->
                    "Name(\"" ++ name ++ "\")"

                Just command ->
                    compileCommand name command

        Ulv.Parser.Exp_Quote body ->
            "Quote({" ++ compileExpressions body ++ "})"

        Ulv.Parser.Exp_Tag (Ulv.Parser.Tag tag) ->
            "Tag(\"" ++ tag ++ "\")"


compileCommand : String -> Ulv.Parser.Command -> String
compileCommand name command =
    let
        commandStr =
            case command of
                Ulv.Parser.Assign ->
                    ".Assign"

                Ulv.Parser.Push ->
                    ".Push"
    in
    "Command{name = Name(\"" ++ name ++ "\"), cmd = " ++ commandStr ++ "}"
