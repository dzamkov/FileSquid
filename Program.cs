using System;
using System.Collections.Generic;

namespace FileSquid
{
    using StringPattern = Pattern<string, string, DictionaryMap<string, object>>;

    /// <summary>
    /// Contains program-related functions.
    /// </summary>
    public static class Program
    {
        /// <summary>
        /// Program main entry-point.
        /// </summary>
        public static void Main(string[] Args)
        {
            StringPattern pattern = Pattern.Concat(new StringPattern[] 
            {
                StringPattern.Literal("root/"),
                StringPattern.Variable("a"),
                StringPattern.Literal("/"),
                StringPattern.Variable("b"),
                StringPattern.Literal("-"),
                StringPattern.Variable("c"),
                StringPattern.Literal(".mp3")
            });
            var matches1 = pattern.Match(DictionaryMap<string, object>.Create(), "root/greetings/hello-world.mp3");
            var matches2 = pattern.Match(DictionaryMap<string, object>.Create(), "root/text.txt");
            var matches3 = pattern.Match(DictionaryMap<string, object>.Create(), "root/a/b/c/d-e-f.mp3");
        }
    }
}
