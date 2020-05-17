namespace Thoth.Json.Fable

open Thoth.Json
open Fable.Core
open Fable.Core.JsInterop

[<RequireQualifiedAccess>]
module Decode =

    type JsonValue = obj

    module Internal =

        [<Emit("JSON.stringify($0, null, 4) + ''")>]
        let inline anyToString (_: JsonValue) : string = jsNative

        [<Emit("isFinite($0) && !($0 % 1)")>]
        let inline isIntFinite (_: JsonValue) : bool = jsNative

        /// is the value an integer? This returns false for 1.1, NaN, Infinite, ...
        [<Emit("isFinite($0) && Math.floor($0) === $0")>]
        let inline isIntegralValue (_: JsonValue) : bool = jsNative

        [<Emit("typeof $0")>]
        let inline jsTypeof (_ : JsonValue) : string = jsNative

        [<Emit("$0 === null ? false : (Object.getPrototypeOf($0 || false) === Object.prototype)")>]
        let inline isObject (_ : JsonValue) : bool = jsNative

        [<Emit("$0 instanceof SyntaxError")>]
        let inline isSyntaxError (_ : JsonValue) : bool = jsNative

    let helpers =
        {
            new IDecoderHelpers<JsonValue> with
                member __.AnyToString(value: JsonValue): string =
                    Internal.anyToString value

                member __.AsArray(value: JsonValue): JsonValue [] =
                    unbox value

                member __.AsBool(value: JsonValue): bool =
                    unbox value

                member __.AsFloat(value: JsonValue): float =
                    unbox value

                member __.AsFloat32(value: JsonValue): float32 =
                    unbox value

                member __.AsInt(value: JsonValue): int =
                    unbox value

                member __.AsString(value: JsonValue): string =
                    unbox value

                member __.GetField(fieldName: string) (value: JsonValue): 'A =
                    value?(fieldName)

                member __.IsArray(value: JsonValue): bool =
                    JS.Constructors.Array.isArray(value)

                member __.IsBoolean(value: JsonValue): bool =
                    value :? bool

                member __.IsIntFinite(value: JsonValue): bool =
                    Internal.isIntFinite value

                member __.IsIntegralValue(value: JsonValue): bool =
                    Internal.isIntegralValue value

                member __.IsNaN(value: JsonValue): bool =
                    JS.Constructors.Number.isNaN(!!value)

                member __.IsNullValue(value: JsonValue): bool =
                    isNull value

                member __.IsNumber(value: JsonValue): bool =
                    Internal.jsTypeof value = "number"

                member __.IsObject(value: JsonValue): bool =
                    Internal.isObject value

                member __.IsString(value: JsonValue): bool =
                    value :? string

                member __.IsUndefined(value: JsonValue): bool =
                    Internal.jsTypeof value = "undefined"

                member __.ObjectKeys(value: JsonValue): seq<string> =
                    upcast JS.Constructors.Object.keys(value)
        }

    ///////////////
    // Runners ///
    /////////////

    let fromValue (decoder : Decoder<JsonValue, 'T>) =
        fun value ->
            match decoder helpers value with
            | Ok success ->
                Ok success
            | Error error ->
                let finalError = error |> Decode.Helpers.prependPath "$"
                Error (Decode.errorToString helpers finalError)

    let fromString (decoder : Decoder<JsonValue, 'T>) =
        fun value ->
            try
                let json = JS.JSON.parse value
                match decoder helpers json with
                | Ok success -> Ok success
                | Error error ->
                    let finalError = error |> Decode.Helpers.prependPath "$"
                    Error (Decode.errorToString helpers finalError)
            with
                | ex when Internal.isSyntaxError ex ->
                    Error("Given an invalid JSON: " + ex.Message)

    let unsafeFromString (decoder : Decoder<JsonValue, 'T>) =
        fun value ->
            match fromString decoder value with
            | Ok x -> x
            | Error msg -> failwith msg
