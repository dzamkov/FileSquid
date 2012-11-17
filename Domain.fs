namespace FileSquid

open System
open System.Collections.Generic

/// Describes a structure that can hold variables.
type [<AbstractClass>] Domain<'a> () =

    /// Gets an empty structure with no variables set.
    abstract Empty : 'a

    /// Gets all the unique variables in this domain.
    abstract Variables : seq<Variable>

/// A domain for an array structure.
type ArrayDomain () =
    inherit Domain<obj[]> ()
    let variables = List<Variable> ()
    override this.Empty = Array.create variables.Count null
    override this.Variables = (variables :> seq<Variable>)

    /// Expands this domain by introducing a new variable.
    member this.Expand () = 
        let var = ArrayVariable<'a> variables.Count
        variables.Add var
        var :> Variable<obj[], 'a>

/// A reference to a variable within an array structure.
and ArrayVariable<'a when 'a : equality> (index : int) =
    inherit Variable<obj[], 'a> ()
    override this.Evaluate array = 
        match array.[index] with
        | null -> None
        | value -> Some (value :?> 'a)
    override this.Match value accum array =
        match array.[index] with
        | null ->
            let nArray = Array.copy array
            nArray.[index] <- (value :> obj)
            nArray :: accum
        | cValue ->
            if value = (cValue :?> 'a) then array :: accum
            else accum