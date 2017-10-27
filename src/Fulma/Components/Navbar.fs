namespace Fulma.Components

open Fulma.BulmaClasses
open Fulma.Common
open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import
open System

module Navbar =

    module Types =

        module Navbar =

            type Option =
                | HasShadow
                | IsTransparent
                | Props of IHTMLProp list
                | CustomClass of string

            type Options =
                { HasShadow : bool
                  IsTransparent : bool
                  CustomClass : string option
                  Props : IHTMLProp list }

                static member Empty =
                    { HasShadow = false
                      IsTransparent = false
                      CustomClass = None
                      Props = [] }


        module Item =

            type Option =
                | IsTab
                | IsActive
                | IsHoverable
                | HasDropdown
                | Props of IHTMLProp list
                | CustomClass of string

            type Options =
                { IsTab : bool
                  IsActive : bool
                  IsHoverable : bool
                  HasDropdown : bool
                  CustomClass : string option
                  Props : IHTMLProp list }

                static member Empty =
                    { IsTab = false
                      IsActive = false
                      IsHoverable = false
                      HasDropdown = false
                      CustomClass = None
                      Props = [] }

        module Link =

            type Option =
                | IsActive
                | Props of IHTMLProp list
                | CustomClass of string

            type Options =
                { IsActive : bool
                  CustomClass : string option
                  Props : IHTMLProp list }

                static member Empty =
                    { IsActive = false
                      CustomClass = None
                      Props = [] }

        module Menu =

            type Option =
                | IsActive
                | Props of IHTMLProp list
                | CustomClass of string

            type Options =
                { IsActive : bool
                  CustomClass : string option
                  Props : IHTMLProp list }

                static member Empty =
                    { IsActive = false
                      CustomClass = None
                      Props = [] }

        module Dropdown =

            type Option =
                | IsActive
                | IsBoxed
                | IsRight
                | Props of IHTMLProp list
                | CustomClass of string


            type Options =
                { IsActive : bool
                  IsBoxed : bool
                  IsRight : bool
                  Props : IHTMLProp list
                  CustomClass : string option }

                static member Empty =
                    { IsActive = false
                      IsBoxed = false
                      IsRight = false
                      Props = []
                      CustomClass = None }

    open Types

    // Helpers definitions

    let hasShadow = Navbar.HasShadow
    let isTransparent = Navbar.IsTransparent
    let props props = Navbar.Props props
    let customClass = Navbar.CustomClass

    module Item =
        let isActive = Item.IsActive
        let isHoverable = Item.IsHoverable
        let isTab = Item.IsTab
        let props props = Item.Props props
        let customClass = Item.CustomClass
        let hasDropdown = Item.HasDropdown


        let item element options children =
            let parseOptions (result: Item.Options ) opt =
                match opt with
                | Item.IsActive -> { result with IsActive = true }
                | Item.IsTab -> { result with IsTab = true }
                | Item.IsHoverable -> { result with IsHoverable = true }
                | Item.HasDropdown -> { result with HasDropdown = true }
                | Item.Props props -> { result with Props = props }
                | Item.CustomClass customClass -> { result with CustomClass = Some customClass }

            let opts = options |> List.fold parseOptions Item.Options.Empty

            element [ yield (classBaseList Bulma.Navbar.Item.Container
                                      [ Bulma.Navbar.Item.State.IsActive, opts.IsActive
                                        Bulma.Navbar.Item.Style.IsTab, opts.IsTab
                                        Bulma.Navbar.Item.IsHoverable, opts.IsHoverable
                                        Bulma.Navbar.Item.Style.HasDropdown, opts.HasDropdown
                                        opts.CustomClass.Value, Option.isSome opts.CustomClass ]) :> IHTMLProp
                      yield! opts.Props ]
                children

    module Link =
        let isActive = Link.IsActive
        let props props = Link.Props props
        let customClass = Link.CustomClass

        let link element (options : Link.Option list) children =
            let parseOptions (result : Link.Options) opt =
                match opt with
                | Link.IsActive -> { result with IsActive = true }
                | Link.CustomClass customClass -> { result with CustomClass = Some customClass}
                | Link.Props props -> { result with Props = props}

            let opts = options |> List.fold parseOptions Link.Options.Empty

            element [ yield (classBaseList Bulma.Navbar.Link.Container
                                       [ Bulma.Navbar.Link.State.IsActive, opts.IsActive ]) :> IHTMLProp
                      yield! opts.Props ]
                children

    module Menu =
        let isActive = Menu.IsActive
        let props props = Menu.Props props
        let customClass = Menu.CustomClass

    module Dropdown =
        let isActive = Dropdown.IsActive
        let isBoxed = Dropdown.IsBoxed
        let isRight = Dropdown.IsRight
        let props = Dropdown.Props
        let customClass = Dropdown.CustomClass

        let dropdown element (options : Dropdown.Option list) children =
            let parseOptions (result : Dropdown.Options) opt =
                match opt with
                | Dropdown.IsActive -> { result with IsActive = true }
                | Dropdown.IsBoxed -> { result with IsBoxed = true }
                | Dropdown.IsRight -> { result with IsRight = true }
                | Dropdown.CustomClass customClass -> { result with CustomClass = Some customClass}
                | Dropdown.Props props -> { result with Props = props}

            let opts = options |> List.fold parseOptions Dropdown.Options.Empty

            element [ yield (classBaseList Bulma.Navbar.Dropdown.Container
                                       [ Bulma.Navbar.Dropdown.IsBoxed, opts.IsBoxed
                                         Bulma.Navbar.Dropdown.IsRight, opts.IsRight
                                         Bulma.Navbar.Dropdown.State.IsActive, opts.IsActive ]) :> IHTMLProp
                      yield! opts.Props ]
                children

    module Brand =
        let brand element (options: GenericOption list) children =
            let opts = genericParse options
            let class' = Helpers.classes Bulma.Navbar.Brand [opts.CustomClass] []
            element (class'::opts.Props) children

    module Start =
        let start element (options: GenericOption list) children =
            let opts = genericParse options
            let class' = Helpers.classes Bulma.Navbar.Start [opts.CustomClass] []
            element (class'::opts.Props) children

    module End =

        let ``end`` element (options: GenericOption list) children =
            let opts = genericParse options
            let class' = Helpers.classes Bulma.Navbar.End [opts.CustomClass] []
            element (class'::opts.Props) children

    let navbar (options : Navbar.Option list) children =
        let parseOptions (result: Navbar.Options ) opt =
            match opt with
            | Navbar.HasShadow -> { result with HasShadow = true }
            | Navbar.Props props -> { result with Props = props }
            | Navbar.IsTransparent -> { result with IsTransparent = true }
            | Navbar.CustomClass customClass -> { result with CustomClass = Some customClass }

        let opts = options |> List.fold parseOptions Navbar.Options.Empty

        nav [ yield (classBaseList Bulma.Navbar.Container
                                   [ Bulma.Navbar.Style.HasShadow, opts.HasShadow
                                     opts.CustomClass.Value, Option.isSome opts.CustomClass
                                     Bulma.Navbar.Style.IsTransparent, opts.IsTransparent]) :> IHTMLProp
              yield! opts.Props ]
            children

    let link_a x y = Link.link a x y
    let link_div x y = Link.link div x y

    let item_a x y = Item.item a x y
    let item_div x y = Item.item div x y

    let dropdown_a x y = Dropdown.dropdown a x y
    let dropdown_div x y = Dropdown.dropdown div x y

    let brand_a x y = Brand.brand a x y
    let brand_div x y = Brand.brand div x y

    let start_a x y = Start.start a x y
    let start_div x y = Start.start div x y

    let end_a x y = End.``end`` a x y
    let end_div x y = End.``end`` div x y

    let menu options children =
        let parseOptions (result: Menu.Options ) opt =
            match opt with
            | Menu.IsActive -> { result with IsActive = true }
            | Menu.Props props -> { result with Props = props }
            | Menu.CustomClass customClass -> { result with CustomClass = Some customClass }

        let opts = options |> List.fold parseOptions Menu.Options.Empty

        div [ yield (classBaseList Bulma.Navbar.Menu.Container
                                    [ Bulma.Navbar.Menu.State.IsActive, opts.IsActive
                                      opts.CustomClass.Value, Option.isSome opts.CustomClass ]) :> IHTMLProp
              yield! opts.Props ]
              children


    let burger (options: GenericOption list) children =
        let opts = genericParse options
        let class' = Helpers.classes Bulma.Navbar.Burger [opts.CustomClass] []
        div (class'::opts.Props) children

    let content (options: GenericOption list) children =
        let opts = genericParse options
        let class' = Helpers.classes Bulma.Navbar.Content [opts.CustomClass] []
        div (class'::opts.Props) children

    let divider (options: GenericOption list) children =
        let opts = genericParse options
        let class' = Helpers.classes Bulma.Navbar.Divider [opts.CustomClass] []
        div (class'::opts.Props) children
