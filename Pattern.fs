namespace FileSquid

open System
open System.Collections.Generic
open System.Text
open FileSquid.Extensions

/// Contains functions for constructing and manipulating patterns.
module Pattern =

    /// A pattern for a string.
    type [<RequireQualifiedAccess>] String<'a> =
        | Variable of Variable<'a, string>
        | Literal of string
        | Concat of String<'a> list

        /// Evaluates a string pattern with the given parameter, returning None if
        /// the result is ambiguous.
        member this.Evaluate parameter =
            match this with
            | Variable var -> var.Get parameter
            | Literal lit -> Some lit
            | Concat parts ->
                let builder = StringBuilder ()
                let rec evaluateParts parts =
                    match parts with
                    | (part : String<'a>) :: parts ->
                        match part.Evaluate parameter with
                        | Some str -> 
                            builder.Append str |> ignore
                            evaluateParts parts
                        | None -> None
                    | [] -> Some (builder.ToString ())
                evaluateParts parts

        /// Finds parameters for the given string pattern that would make it equivalent to
        /// the given target string, appending them to the given list.
        member this.Apply target accum initial =
            match this with
            | Variable var -> var.Set target accum initial
            | Literal lit -> if target = lit then initial :: accum else accum
            | Concat parts ->
                let rec applyParts parts index accum initial  =
                    match parts with
                    | part :: Literal suffix :: [] ->
                        let partEnd = target.Length - suffix.Length
                        let hasSuffix = (partEnd >= 0 && String.substringEquals suffix 0 target partEnd suffix.Length)
                        if hasSuffix then part.Apply (String.between target index partEnd) accum initial
                        else accum
                    | part :: Literal delimiter :: parts ->
                        match String.findOccurences target delimiter index with
                        | [] -> accum
                        | partEnd :: [] -> 
                            match part.Apply (String.between target index partEnd) [] initial with
                            | [] -> accum
                            | state :: [] -> applyParts parts (partEnd + delimiter.Length) accum state
                            | states -> List.fold (applyParts parts (partEnd + delimiter.Length)) accum states
                        | partEnds ->
                            let mutable accum = accum
                            for partEnd in partEnds do 
                                for parameter in part.Apply (String.between target index partEnd) [] initial do
                                    accum <- applyParts parts  (partEnd + delimiter.Length) accum parameter
                            accum
                    | part :: [] -> part.Apply (String.between target index target.Length) accum initial
                    | part :: parts ->
                        let mutable accum = accum
                        for partEnd = index to target.Length do
                            for parameter in part.Apply (String.between target index partEnd) [] initial do
                                accum <- applyParts parts partEnd accum parameter
                        accum
                    | [] when index = target.Length -> initial :: accum
                    | [] -> accum
                match parts with
                | Literal prefix :: parts ->
                    let hasPrefix = (prefix.Length <= target.Length && String.substringEquals prefix 0 target 0 prefix.Length)
                    if hasPrefix then applyParts parts prefix.Length [] initial
                    else []
                | parts -> applyParts parts 0 [] initial

    /// Contains functions for constructing string patterns.
    module String =

        /// Creates a string variable pattern.
        let var var = String.Variable var

        /// Creates a string literal pattern.
        let lit lit = String.Literal lit

        /// Creates a string concatenation pattern.
        let concat parts =
            let rec concat (literalString : string) accum parts =
                let forceAccum () = 
                    if literalString.Length > 0 then String.Literal literalString :: accum
                    else accum
                match parts with
                | String.Literal lit :: parts -> concat (literalString + lit) accum parts
                | String.Concat nParts :: parts -> concat literalString accum (List.append nParts parts)
                | part :: parts -> concat "" (part :: forceAccum ()) parts
                | [] -> 
                    match forceAccum () with
                    | [] -> String.Literal ""
                    | accum -> String<'a>.Concat (List.rev accum)
            concat "" [] parts