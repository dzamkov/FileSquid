namespace FileSquid

open System
open System.Collections.Generic
open System.Text
open FileSquid.Extensions

/// Contains functions for constructing and manipulating patterns.
module Pattern =

    /// A function that applies a pattern to a value.
    type Apply<'a, 'b> = 'a -> 'b list -> 'b -> 'b list

    /// A pattern for a string.
    type [<RequireQualifiedAccess>] String<'a> =
        | Variable of Variable<'a, string>
        | Literal of string
        | Concat of String<'a> list

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

        /// Evaluates a string pattern with the given parameter, returning None if
        /// the result is ambiguous.
        let rec evaluate pattern parameter =
            match pattern with
            | String.Variable var -> var.Get parameter
            | String.Literal lit -> Some lit
            | String.Concat parts ->
                let builder = StringBuilder ()
                let rec evaluateParts parts =
                    match parts with
                    | (part : String<'a>) :: parts ->
                        match evaluate part parameter with
                        | Some str -> 
                            builder.Append str |> ignore
                            evaluateParts parts
                        | None -> None
                    | [] -> Some (builder.ToString ())
                evaluateParts parts


        /// Matches a simple string pattern that has a known value when using the given structure.
        let (|Known|_|) (initial : 'a) (pattern : String<'a>) =
            match pattern with
            | String.Literal lit -> Some lit
            | String.Variable var -> var.Get initial
            | _ -> None

        /// Finds parameters for the given string pattern that would make it equivalent to
        /// the given target string, appending them to the given list.
        let rec apply pattern target accum initial =
            match pattern with
            | String.Variable var -> var.Set target accum initial
            | String.Literal lit -> if target = lit then initial :: accum else accum
            | String.Concat parts ->
                let rec applyParts parts index accum initial  =
                    match parts with
                    | Known initial literal :: parts ->
                        let hasLiteral = (literal.Length <= target.Length - index && String.substringEquals literal 0 target index literal.Length)
                        if hasLiteral then applyParts parts (index + literal.Length) accum initial
                        else accum
                    | part :: Known initial suffix :: [] ->
                        let partEnd = target.Length - suffix.Length
                        let hasSuffix = (partEnd >= index && String.substringEquals suffix 0 target partEnd suffix.Length)
                        if hasSuffix then apply part (String.between target index partEnd) accum initial
                        else accum
                    | part :: Known initial delimiter :: parts ->
                        match String.findOccurences target delimiter index with
                        | [] -> accum
                        | partEnd :: [] -> 
                            match apply part (String.between target index partEnd) [] initial with
                            | [] -> accum
                            | state :: [] -> applyParts parts (partEnd + delimiter.Length) accum state
                            | states -> List.fold (applyParts parts (partEnd + delimiter.Length)) accum states
                        | partEnds ->
                            let mutable accum = accum
                            for partEnd in partEnds do 
                                for parameter in apply part (String.between target index partEnd) [] initial do
                                    accum <- applyParts parts  (partEnd + delimiter.Length) accum parameter
                            accum
                    | part :: [] -> apply part (String.between target index target.Length) accum initial
                    | part :: parts ->
                        let mutable accum = accum
                        for partEnd = index to target.Length do
                            for parameter in apply part (String.between target index partEnd) [] initial do
                                accum <- applyParts parts partEnd accum parameter
                        accum
                    | [] when index = target.Length -> initial :: accum
                    | [] -> accum
                applyParts parts 0 [] initial