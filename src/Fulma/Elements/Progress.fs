namespace Fulma.Elements

open Fulma.BulmaClasses
open Fulma.Common
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props

module Progress =
    module Types =
        type Option =
            | Size of ISize
            | Color of ILevelAndColor
            | Props of IHTMLProp list
            | Value of int
            | Max of int
            | CustomClass of string

        type Options =
            { Size : string option
              Color : string option
              Props : IHTMLProp list
              Max : int option
              Value : int option
              CustomClass : string option }
            static member Empty =
                { Size = None
                  Color = None
                  Props = []
                  Max = None
                  Value = None
                  CustomClass = None }

    open Types

    // Sizes
    let isSmall = Size IsSmall
    let isMedium = Size IsMedium
    let isLarge = Size IsLarge
    // Levels and colors
    let isBlack = Color IsBlack
    let isDark = Color IsDark
    let isLight = Color IsLight
    let isWhite = Color IsWhite
    let isPrimary = Color IsPrimary
    let isInfo = Color IsInfo
    let isSuccess = Color IsSuccess
    let isWarning = Color IsWarning
    let isDanger = Color IsDanger
    // Extra
    let props props = Props props
    let value v = Value v
    let max m = Max m
    let customClass = CustomClass

    let progress options children =
        let parseOptions (result : Options) =
            function
            | Size size -> { result with Size = ofSize size |> Some }
            | Color color -> { result with Color = ofLevelAndColor color |> Some }
            | Props props -> { result with Props = props }
            | Value value -> { result with Value = value |> Some }
            | Max max -> { result with Max = max |> Some }
            | CustomClass customClass -> { result with CustomClass = customClass |> Some }

        let opts = options |> List.fold parseOptions Options.Empty
        progress
            [ yield ClassName (Helpers.generateClassName Bulma.Progress.Container [ opts.Size; opts.Color; opts.CustomClass ]) :> IHTMLProp
              yield! opts.Props
              if Option.isSome opts.Value then yield HTMLAttr.Value (string opts.Value.Value) :> IHTMLProp
              if Option.isSome opts.Max then yield HTMLAttr.Max (float opts.Max.Value) :> IHTMLProp ]
            children
