package anagrams

/**
 * Sentence anagrams generation
 * MSE course, T-AdvPrPa course
 */
object Anagrams {
  
	/**
	 * Types definitions
	 */
	type Word = String
	type Occurrences = List[(Char, Int)]
	type Sentence = List[Word]

	/**
	 * The dictionary is simply a sequence of words.
	 * It is predefined and obtained as a sequence using the utility method `loadDictionary`.
	 */
	val dictionary: List[Word] = loadDictionary()

  def transformCharMapToOccurences( map : Map[Char,List[Char]], existingList : Occurrences ) : Occurrences =
    if( map == Nil )
      existingList
    else {
      val (c, l) = map.head
      transformCharMapToOccurences( map.tail, ( c, l.length )::existingList )
    }
    
	/**
	 *  Question 1 : converts the word into its character occurrence list.
	 *
	 *  For instance,
	 *  wordOccurences("abcd") gives List(('a', 1), ('b', 1), ('c', 1), ('d', 1))
	 *  wordOccurences("aabcddd") gives List(('a', 2), ('b', 1), ('c', 1), ('d', 3))
	 *
	 *  Note: the upper case and lower case version of the character are treated as the
	 *  same character, and are represented as a lower case character in the occurrence list.
	 */
	def wordOccurrences(w: Word): Occurrences =
    transformCharMapToOccurences( w.toLowerCase.toList.groupBy( s => s ), Nil ).sortWith(_._1 < _._1)

	/**
	 * Question 2: Valid words
	 * 
	 * The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
	 * the words that have that occurrence count. This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
	 */
	lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = ???

	/**
	 * Question 3: Returns all the anagrams of a given word
	 */	
	def wordAnagrams(word: Word): List[Word] = ???
	
	/**
	 * Question 4: Returns all the subsets of an occurrence list
	 */

	/**
	 * Generates all the subsets of a set
	 */
	def combinations(occurrences: Occurrences): List[Occurrences] = ???
	
	/**
	 * Question 5: remove occurrences from x that are in y
	 */
	def subtract(x: Occurrences, y: Occurrences): Occurrences = ???
	
	/**
	 * Question 6 - Generate sentence anagrams
	 */
	
	/** Converts a sentence into its character occurrence list. */
	def sentenceOccurrences(s: Sentence): Occurrences = ???
	
	def sentenceAnagrams(sentence: Sentence): List[Sentence] = ???

}
