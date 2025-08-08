module Ulv.Odin exposing (compile)

import Ulv.Parser


compile : Bool -> List Ulv.Parser.Expression -> String
compile debugMode expressions =
    """package ulv

import "core:fmt"
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

        append(&env.stack, comp == Tag("LT"))
    }})
    append(&env.dict, Word{ label = ">", value = proc(env: ^Env) {
        right := pop(&env.stack)
        left := pop(&env.stack)

        comp := internal_compare(&left, &right)

        append(&env.stack, comp == Tag("GT"))
    }})
    append(&env.dict, Word{ label = "<=", value = proc(env: ^Env) {
        right := pop(&env.stack)
        left := pop(&env.stack)

        comp := internal_compare(&left, &right)

        append(&env.stack, comp == Tag("LT") || comp == Tag("EQ"))
    }})
    append(&env.dict, Word{ label = ">=", value = proc(env: ^Env) {
        right := pop(&env.stack)
        left := pop(&env.stack)

        comp := internal_compare(&left, &right)

        append(&env.stack, comp == Tag("GT") || comp == Tag("EQ"))
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

    // RUN PROGRAM
    for value in compiled_values {
        eval(&env, value)
    }
}

internal_compare :: proc(left, right: ^Value) -> Tag {
    #partial switch r in right {
    case bool:
        panic("Booleans are equatable but not comparable")
    case int:
        #partial switch l in left {
        case int:
            if  l == r {
                return Tag("EQ")
            } else if l < r {
                return Tag("LT")
            } else {
                return Tag("GT")
            }
        }
    case f64:
        #partial switch l in left {
        case f64:
            if  l == r {
                return Tag("EQ")
            } else if l < r {
                return Tag("LT")
            } else {
                return Tag("GT")
            }
        }
    case string:
        #partial switch l in left {
        case string:
            if  l == r {
                return Tag("EQ")
            } else if l < r {
                return Tag("LT")
            } else {
                return Tag("GT")
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

                if comp == Tag("LT") || comp == Tag("GT") {
                    return comp
                }
            }

            if len(l) == len(r) {
                return Tag("EQ")
            } else if len(l) < len(r) {
                return Tag("LT")
            } else {
                return Tag("GT")
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
            panic("Unknown word!")
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
                panic("Unknown word!")
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
            "string(\"" ++ string ++ "\")"

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
