namespace FileSquid

open System
open System.Collections.Generic
open System.Text
open FileSquid.Util

/// Describes the general form of a value.
type Pattern =
    interface
    end

/// Describes the general form of a value of a certain type.
type [<AbstractClass>] Pattern<'a, 'b when 'b : equality> () =
    interface Pattern
    
    /// Evaluates this pattern for a given parameter, returning None if any of the variables
    /// are not set or set to an invalid value.
    abstract Evaluate : 'a -> 'b option

    /// Finds parameters for this pattern that would make it equivalent to the given target value, 
    /// appending them to the given list.
    abstract Match : 'b -> 'a list -> 'a  -> 'a list

    /// Substitutes the variables in this pattern using the given mapping function.
    abstract Substitute : (Variable -> Pattern) -> Pattern<'a, 'b>

/// A reference to a variable value within a parameter structure. 
and Variable =
    interface
    end

/// A reference to a typed variable value within a parameter structure.
type [<AbstractClass>] Variable<'a, 'b when 'b : equality> () =
    inherit Pattern<'a, 'b> ()
    interface Variable
    override this.Substitute map = (map this) :?> Pattern<'a, 'b>

/// A pattern with a specific value.
type Literal<'a, 'b when 'b : equality> (value : 'b) =
    inherit Pattern<'a, 'b> ()

    /// The value for this literal.
    member this.Value = value

    override this.Evaluate _ = Some value
    override this.Match target accum initial = 
        if value = target then initial :: accum
        else accum
    override this.Substitute _ = this :> Pattern<'a, 'b>

/// A pattern for a concatenation of strings.
type Concat<'a> private (parts : Pattern<'a, string> list) =
    inherit Pattern<'a, string> ()
    let (|Known|_|) initial (pattern : Pattern<'a, string>) = pattern.Evaluate initial

    /// Creates a concatenation pattern for the given parts.
    static member Create parts =
        let rec concat (literalString : string) accum parts =
            let forceAccum () = 
                if literalString.Length > 0 then (Literal<'a, string> literalString :> Pattern<'a, string>) :: accum
                else accum
            match (parts : Pattern<'a, string> list) with
            | :? Literal<'a, string> as part :: parts -> concat (literalString + part.Value) accum parts
            | :? Concat<'a> as part :: parts -> concat literalString accum (List.append part.Parts parts)
            | part :: parts -> concat "" (part :: forceAccum ()) parts
            | [] -> 
                match forceAccum () with
                | [] -> Literal<'a, string> "" :> Pattern<'a, string>
                | accum -> Concat<'a> (List.rev accum) :> Pattern<'a, string>
        concat "" [] parts

    /// The parts for this pattern.
    member this.Parts = parts

    override this.Evaluate parameter =
        let builder = StringBuilder ()
        let rec evaluateParts parts =
            match parts with
            | (part : Pattern<'a, string>) :: parts ->
                match part.Evaluate parameter with
                | Some str -> 
                    builder.Append str |> ignore
                    evaluateParts parts
                | None -> None
            | [] -> Some (builder.ToString ())
        evaluateParts parts
    override this.Match target accum initial =
        let rec applyParts parts index accum initial  =
            match parts with
            | Known initial literal :: parts ->
                let hasLiteral = (literal.Length <= target.Length - index && String.substringEquals literal 0 target index literal.Length)
                if hasLiteral then applyParts parts (index + literal.Length) accum initial
                else accum
            | part :: Known initial suffix :: [] ->
                let partEnd = target.Length - suffix.Length
                let hasSuffix = (partEnd >= index && String.substringEquals suffix 0 target partEnd suffix.Length)
                if hasSuffix then part.Match (String.between target index partEnd) accum initial
                else accum
            | part :: Known initial delimiter :: parts ->
                match String.findOccurences target delimiter index with
                | [] -> accum
                | partEnd :: [] -> 
                    match part.Match (String.between target index partEnd) [] initial with
                    | [] -> accum
                    | state :: [] -> applyParts parts (partEnd + delimiter.Length) accum state
                    | states -> List.fold (applyParts parts (partEnd + delimiter.Length)) accum states
                | partEnds ->
                    let mutable accum = accum
                    for partEnd in partEnds do 
                        for parameter in part.Match (String.between target index partEnd) [] initial do
                            accum <- applyParts parts  (partEnd + delimiter.Length) accum parameter
                    accum
            | part :: [] -> part.Match (String.between target index target.Length) accum initial
            | part :: parts ->
                let mutable accum = accum
                for partEnd = index to target.Length do
                    for parameter in part.Match (String.between target index partEnd) [] initial do
                        accum <- applyParts parts partEnd accum parameter
                accum
            | [] when index = target.Length -> initial :: accum
            | [] -> accum
        applyParts parts 0 [] initial
    override this.Substitute map = Concat<'a>.Create (parts |> List.map (fun part -> part.Substitute map))

/// Contains functions for constructing and manipulating patterns.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Pattern =

    /// Creates a literal pattern.
    let literal value = Literal<'a, 'b> value :> Pattern<'a, 'b>

    /// Creates a string concatenation pattern.
    let concat parts = Concat<'a>.Create parts

    /// Substitutes the variables in the given pattern.
    let substitute map (pattern : Pattern<'a, 'b>) = pattern.Substitute map