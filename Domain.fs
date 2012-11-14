namespace FileSquid

/// Describes a structure that can hold variables.
type [<AbstractClass>] Domain<'a> () =

    /// Gets an empty structure with no values set.
    abstract Empty : 'a

/// A domain for an array structure.
type ArrayDomain () =
    inherit Domain<obj[]> ()
    let mutable size = 0
    override this.Empty = Array.create size null

    /// Expands this domain by introducing a new variable.
    member this.Expand () = 
        let var = ArrayVariable<'a> size
        size <- size + 1
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