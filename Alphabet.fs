namespace FileSquid

/// A set of symbols where each symbol is paired with an integer index.
type [<AbstractClass>] Alphabet<'a> (size) =
    
    /// Gets the amount of symbols in this alphabet.
    member this.Size = size

    /// Determines whether the given symbol is in this alphabet.
    abstract Contains : 'a -> bool

    /// Gets the symbol for the given index.
    abstract SymbolFor : int -> 'a

    /// Gets the index for the given symbols.
    abstract IndexFor : 'a -> int

    /// Gets the symbol for the given index.
    member this.Item with get index = this.SymbolFor index

    /// Gets the index for the given symbol.
    member this.Item with get symbol = this.IndexFor symbol

/// An alphabet for the non-control ASCII characters.
type ASCIIAlphabet private () =
    inherit Alphabet<char> (int '~' - int ' ')
    static let instance = ASCIIAlphabet ()

    /// The only instance of this class.
    static member Instance = instance

    override this.Contains ch = (ch >= ' ' && ch <= '~')
    override this.SymbolFor index = char (index + int ' ')
    override this.IndexFor (ch : char) = int ch - int ' '

/// An alphabet for a range of consecutive integers.
type RangeAlphabet (first : int, size : int) =
    inherit Alphabet<int> (size)
    override this.Contains index =
        let index = index - first
        index >= 0 && index < size
    override this.SymbolFor index = first + index
    override this.IndexFor index = index - first

/// Contains functions for constructing and manipulating alphabets.
module Alphabet =

    /// The ASCII alphabet.
    let ascii = ASCIIAlphabet.Instance :> Alphabet<char>

    /// Constructs a range alphabet.
    let range first size = RangeAlphabet (first, size) :> Alphabet<int>