namespace Fulma.Elements

open Fulma.BulmaClasses
open Fulma.Common
open Fable.Core
open Fable.Helpers.React
open Fable.Helpers.React.Props

module Content =
    module Types =
        type Option =
            | Size of ISize
            | CustomClass of string
            | Props of IHTMLProp list

        type Options =
            { Size : string option
              Props : IHTMLProp list
              CustomClass : string option }
            static member Empty =
                { Size = None
                  Props = []
                  CustomClass = None }

    open Types

    // Sizes
    let isSmall = Size IsSmall
    let isMedium = Size IsMedium
    let isLarge = Size IsLarge
    // Extra
    let props = Props
    let customClass = CustomClass

    let content (options : Option list) children =
        let parseOption (result : Options) opt =
            match opt with
            | Size size -> { result with Size = ofSize size |> Some }
            | CustomClass customClass -> { result with CustomClass = Some customClass }
            | Props props -> { result with Props = props }

        let opts = options |> List.fold parseOption Options.Empty
        div
            [ yield classBaseList
                        Bulma.Content.Container
                        [ opts.CustomClass.Value, Option.isSome opts.CustomClass
                          opts.Size.Value, Option.isSome opts.Size ] :> IHTMLProp
              yield! opts.Props ]
            children
