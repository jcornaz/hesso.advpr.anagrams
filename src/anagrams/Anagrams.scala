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
	def wordOccurrences(w: Word): Occurrences = {
    val grouped = w.toLowerCase.toList.groupBy( s => s )
    val transformed = grouped map { case (key,value) => (key, value.length) }
    transformed.toList.sortWith( _._1 < _._1 )
  }
  
  def createOccurrencesWordMap( words : List[Word], map : Map[Occurrences,List[Word]] ) : Map[Occurrences, List[Word]] = {
    if( words == Nil )
      map
    else {
      val word = words.head
      val occurrences = wordOccurrences( word )
      if( !map.contains( occurrences ) )
        createOccurrencesWordMap( words.tail, map + (occurrences -> List(word) ) )
      else {
        val list = word::map.get(occurrences).get
        createOccurrencesWordMap( words. tail, (map - occurrences) + (occurrences -> list))
      }
    }
  }

	/**
	 * Question 2: Valid words
	 * 
	 * The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
	 * the words that have that occurrence count. This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
	 */
	lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
			createOccurrencesWordMap( dictionary, Map[Occurrences,List[Word]]() )

	/**
	 * Question 3: Returns all the anagrams of a given word
	 */	
	def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))
	
	/**
	 * Question 4: Returns all the subsets of an occurrence list
	 */

  /**
   * Generates the subset for one occurrence.
   * 
   * For instance,
   * eltCombinations(('c',3)) give List(('c',3'),('c',2),('c',1))
   */
  def eltCombinations( occurrence : (Char, Int) ) : List[(Char,Int)] = {
    if( occurrence._2 == 0 )
      List[(Char,Int)]()
    else
      occurrence::eltCombinations( (occurrence._1, occurrence._2 - 1) )
  }
  
  def appendElement[T]( lst : List[T], elt : T) : List[T] = 
    (elt::lst.reverse).reverse
  
  def append[T]( lst1 : List[T], lst2 : List[T] ) : List[T] = {
    if( lst1 == Nil )
      lst2
    else if( lst2 == Nil )
      lst1
    else
      append( appendElement( lst1, lst2.head ), lst2.tail )
  }
  
	/**
	 * Generates all the subsets of a set
	 */
	def combinations(occurrences: Occurrences) : List[Occurrences] = {
    if( occurrences == Nil )
      List[Occurrences](List[(Char,Int)]())
    else {
      val head = eltCombinations( occurrences.head )
      val tail = combinations( occurrences.tail )
      val c = tail.map( (lst : Occurrences) => head.map( (pair : (Char,Int)) => pair::lst ) ).flatten
      append( c, tail )
    }
  }
	
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
