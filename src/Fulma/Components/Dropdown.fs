namespace Fulma.Components

open Fulma.BulmaClasses
open Fulma.Common
open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import

module Dropdown =

    module Types =

        type Option =
            | IsActive
            | IsHoverable
            | IsRight
            | Props of IHTMLProp list
            | CustomClass of string

        type Options =
            { Props : IHTMLProp list
              IsActive : bool
              IsHoverable : bool
              IsRight : bool
              CustomClass : string option }

            static member Empty =
                { Props = []
                  IsActive = false
                  IsHoverable = false
                  IsRight = false
                  CustomClass = None }

        module Item =

            type Option =
                | IsActive
                | Props of IHTMLProp list
                | CustomClass of string

            type Options =
                { Props : IHTMLProp list
                  IsActive : bool
                  CustomClass : string option }

                static member Empty =
                    { Props = []
                      IsActive = false
                      CustomClass = None }

    open Types

    let customClass = CustomClass
    let props = Props
    let isActive = IsActive
    let isHoverable = IsHoverable
    let isRight = IsRight

    module Menu =
        let customClass = CustomClass
        let props = Props

    module Content =
        let customClass = CustomClass
        let props = Props

    module Divider =
        let customClass = CustomClass
        let props = Props

    module Item =
        let customClass = Item.CustomClass
        let props = Item.Props
        let isActive = Item.IsActive

    let dropdown (options: Option list) children =
        let parseOptions (result : Options) =
            function
            | IsActive -> { result with IsActive = true }
            | IsRight -> { result with IsRight = true }
            | IsHoverable -> { result with IsHoverable = true }
            | Props props -> { result with Props = props }
            | CustomClass customClass -> { result with CustomClass = Some customClass }

        let opts = options |> List.fold parseOptions Options.Empty
        let class' =
            [ Bulma.Dropdown.Alignment.IsRight, opts.IsRight
              Bulma.Dropdown.State.IsActive, opts.IsActive
              Bulma.Dropdown.State.IsHoverable, opts.IsHoverable ]
            |> Helpers.classes Bulma.Dropdown.Container [opts.CustomClass]

        div (class'::opts.Props) children

    let menu (options: GenericOption list) children =
        let opts = genericParse options
        let class' = Helpers.classes Bulma.Dropdown.Menu [opts.CustomClass] []
        div (class'::opts.Props) children

    let content (options: GenericOption list) children =
        let opts = genericParse options
        let class' = Helpers.classes Bulma.Dropdown.Content [opts.CustomClass] []
        div (class'::opts.Props) children

    let divider (options: GenericOption list) =
        let opts = genericParse options
        let class' = Helpers.classes Bulma.Dropdown.Divider [opts.CustomClass] []
        hr (class'::opts.Props)

    let item (options: Item.Option list) children =
        let parseOptions (result : Item.Options) =
            function
            | Item.IsActive -> { result with IsActive = true }
            | Item.Props props -> { result with Props = props }
            | Item.CustomClass customClass -> { result with CustomClass = Some customClass }

        let opts = options |> List.fold parseOptions Item.Options.Empty
        let class' =
            [ Bulma.Dropdown.Item.State.IsActive, opts.IsActive ]
            |> Helpers.classes Bulma.Dropdown.Item.Container [opts.CustomClass]

        a (class'::opts.Props) children
