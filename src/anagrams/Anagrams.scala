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
  
  /**
   * Create a map, with occurrences as keys and corresponding word list as value
   */
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
    if( occurrence._2 == 1 )
      List[(Char,Int)]( occurrence )
    else
      occurrence::eltCombinations( (occurrence._1, occurrence._2 - 1) )
  }
  
  /**
   * Generates all the subsets of a set
   */
  def combinations(occurrences: Occurrences) : List[Occurrences] = {
    if( occurrences == Nil )
      List[Occurrences](List[(Char,Int)]())
    else {
      val heads = eltCombinations( occurrences.head )
      val tails = combinations( occurrences.tail )
      tails.foldLeft( List[Occurrences]() )( (res1, tail) => {
        tail::heads.foldLeft( res1 )( (res2, head) => (head::tail)::res2 )
      })
    }
  }
	
  
	/**
	 * Question 5: remove occurrences from x that are in y
	 */
  
  /**
   * Transform an instance of Occurrences in a map
   */
  def occurrencesToMap( occurrences : Occurrences, res : Map[Char,Int] ) : Map[Char,Int] = {
     if( occurrences == Nil )
       res
     else {
       val (key, value) = occurrences.head
       occurrencesToMap( occurrences.tail, res + (key -> value) )
     }
  }
  
  /**
   * Deduce from x the occurrences in y
   */
	def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val ymap = occurrencesToMap( y, Map[Char,Int]() )
    x.foldRight(List[(Char,Int)]())( (pair, lst) => {
      if( ymap.contains( pair._1 ) )
         if( ymap( pair._1 ) >= pair._2 )
           lst
         else
           (pair._1, pair._2 - ymap( pair._1 ) )::lst
      else
        pair::lst
    })
  }
  
     
	/**
	 * Question 6 - Generate sentence anagrams
	 */

	/** Converts a sentence into its character occurrence list. */
	def sentenceOccurrences(s: Sentence): Occurrences = {
    if( s == Nil )
      List[(Char,Int)]()
    else
      wordOccurrences( s.reduce( _.concat( _ ) ) )
  }
	
  /**
   * Merge two lists together
   */
  def merge[T]( lst1 : List[T], lst2 : List[T] ) : List[T] = {
    if( lst1 == Nil )
      lst2
    else if( lst2 == Nil )
      lst1
    else
      lst1.foldRight( lst2 )( (elt,lst) => elt::lst )
  }
  
  /**
   * Compute the list of the sentence which have a given end and a given set of occurrence to use for the beginning
   */
  def completeSentenceAnagrams( occurrences : Occurrences, end : Sentence ) : List[Sentence] = {
    if( occurrences == Nil )
      List[Sentence](end)
    else {
      combinations( occurrences ).foldLeft( List[Sentence]() )( (res, subset) => {
        if( dictionaryByOccurrences.contains( subset ) )
          dictionaryByOccurrences( subset ).foldLeft( res )( ( sentences, word ) => {
            merge( sentences, completeSentenceAnagrams( subtract( occurrences, subset ), word::end ) )
          })
        else
          res
      })
    }
  }
  
	def sentenceAnagrams(sentence: Sentence): List[Sentence] =
    completeSentenceAnagrams( sentenceOccurrences( sentence ), List[Word]() )
}
