namespace Fulma.Elements

open Fulma.BulmaClasses
open Fable.Core
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma.Common

module Box =

    let customClass cls = CustomClass cls
    let props props = Props props

    let box' (options: GenericOption list) children =
        let opts = genericParse options
        let class' = Helpers.classes Bulma.Box.Container [opts.CustomClass] []
        div (class'::opts.Props) children
