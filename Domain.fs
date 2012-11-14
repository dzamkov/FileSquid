namespace FileSquid

/// A reference to a typed variable value within a structure.
type [<AbstractClass>] Variable<'a, 'b when 'b : equality> () =

    /// Gets the value for this variable within the given structure, or None
    /// if it is not set.
    abstract Get : 'a -> 'b option

    /// Sets the value for this variable within the given structure, prepending the
    /// modified structure to the given list.
    abstract Set : 'b -> 'a list -> 'a  -> 'a list

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
    override this.Get array = 
        match array.[index] with
        | null -> None
        | value -> Some (value :?> 'a)
    override this.Set value accum array =
        match array.[index] with
        | null ->
            let nArray = Array.copy array
            nArray.[index] <- (value :> obj)
            nArray :: accum
        | cValue ->
            if value = (cValue :?> 'a) then array :: accum
            else accum