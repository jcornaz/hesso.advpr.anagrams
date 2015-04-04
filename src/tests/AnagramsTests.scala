package tests

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import anagrams.Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsTests extends FunSuite {
	/**
	 * Test suite for question 1
	 */
	test("wordOccurrences: empty string") {
		assert(wordOccurrences("") === List())
	}

	test("wordOccurrences: \"abcd\"") {
		assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
	}

	test("wordOccurrences: \"aabcdeee\"") {
		assert(wordOccurrences("aabcdeee") === List(('a', 2), ('b', 1), ('c', 1), ('d', 1), ('e', 3)))
	}

	test("wordOccurrences: \"Robert\"") {
		assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
	}
	
	/***
	 * Test suite for question 2
	 */
	test("""dictionaryByOccurrences for "eat" """) {
		assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
	}
	
	/***
	 * Test suite for question 3
	 */
	test("""wordAnagrams for "married" """) {
		assert(wordAnagrams("married").toSet === Set("married", "admirer"))
	}

	test("""wordAnagrams for "mushroom" """) {
		assert(wordAnagrams("mushroom").toSet === Set("mushroom"))
	}

	test("""wordAnagrams for "player" """) {
		assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
	}
	
	/**
	 * Test suite for question 4
	 */
	test("combinations: empty combination []") {
		assert(combinations(Nil) === List(Nil))
	}	

	test("combinations: \"aa\"") {
		val aa = List(('a', 2))
		val aacomb = List(
			List(),
			List(('a', 1)),
			List(('a', 2))
		)
    val res = combinations(aa)
    assert(res.length == aacomb.length )
		assert(res.toSet === aacomb.toSet)
	}
	
	test("combinations: \"abba\"") {
		val abba = List(('a', 2), ('b', 2))
		val abbacomb = List(
			List(),
			List(('a', 1)),
			List(('a', 2)),
			List(('b', 1)),
			List(('a', 1), ('b', 1)),
			List(('a', 2), ('b', 1)),
			List(('b', 2)),
			List(('a', 1), ('b', 2)),
			List(('a', 2), ('b', 2))
    )
    val res = combinations(abba)
    assert(res.length == abbacomb.length )
		assert(res.toSet === abbacomb.toSet)
	}
	
	/**
	 * Test suite for question 5
	 */
	test("subtract: \"lard\" - \"r\"") {
		val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
		val r = List(('r', 1))
		val lad = List(('a', 1), ('d', 1), ('l', 1))
		assert(subtract(lard, r) === lad)
	}
	
	test("subtract: \"lard\" - \"\"") {
		val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
		val empty = List()		
		assert(subtract(lard, empty) === lard)
	}
	
	test("subtract: \"lard\" - \"lard\"") {
		val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))				
		assert(subtract(lard, lard) === List())
	}
  
  test("subtract: \"jonathan\" - \"han\"") {
    val jonathan = List(('a',2), ('h', 1), ('j', 1), ('n', 2), ('o', 1), ('t', 1))
    val han = List(('a',1),('h',1),('n', 1))
    val jonat = List(('a',1), ('j', 1), ('n', 1), ('o', 1), ('t', 1))
    assert( subtract( jonathan, han ) === jonat )
  }
	
	/**
	 * Test suite for question 6
	 */
	test("sentence anagrams: []") {
		val sentence = List()
		assert(sentenceAnagrams(sentence) === List(Nil))
	}

	test("sentence anagrams: Linux rulez") {
		val sentence = List("Linux", "rulez")
		val anas = List(
			List("Rex", "Lin", "Zulu"),
			List("nil", "Zulu", "Rex"),
			List("Rex", "nil", "Zulu"),
			List("Zulu", "Rex", "Lin"),
			List("null", "Uzi", "Rex"),
			List("Rex", "Zulu", "Lin"),
			List("Uzi", "null", "Rex"),
			List("Rex", "null", "Uzi"),
			List("null", "Rex", "Uzi"),
			List("Lin", "Rex", "Zulu"),
			List("nil", "Rex", "Zulu"),
			List("Rex", "Uzi", "null"),
			List("Rex", "Zulu", "nil"),
			List("Zulu", "Rex", "nil"),
			List("Zulu", "Lin", "Rex"),
			List("Lin", "Zulu", "Rex"),
			List("Uzi", "Rex", "null"),
			List("Zulu", "nil", "Rex"),
			List("rulez", "Linux"),
			List("Linux", "rulez")
    )
    val res = sentenceAnagrams(sentence)
    assert(res.length == anas.length )
		assert(res.toSet === anas.toSet)
	}
  
  test("sentence anagrams: I love you") {
    val sentence = List("I", "Love", "You")
    val anas = List(
        List("you", "olive"),
        List("Io", "you", "Lev"),
        List("you", "Io", "Lev"),
        List("Lev", "you", "Io"),
        List("you", "Lev", "Io"),
        List("Lev", "Io", "you"),
        List("olive", "you"),
        List("Io", "Lev", "you")
    )
    val res = sentenceAnagrams( sentence )
    assert( res.length == anas.length )
    assert( res.toSet === anas.toSet )
  }
  
  test("sentence anagrams: debit card") {
    val sentence = List( "debit", "card" )
    val anas = sentenceAnagrams( sentence )
    assert( anas.length == 242 )
    assert( anas.contains( List("bad", "credit" ) ) )
  }
  
  test("sentence anagrams: Mickey Mouse") {
    val sentence = List( "Mickey", "Mouse" )
    val anas = sentenceAnagrams( sentence )
    assert( anas.length == 1154 )
    assert( anas.contains( List("you", "me", "sick", "me" ) ) )
    assert( anas.contains( List("Ku", "mice", "soy", "em" ) ) )
  }
  
  test("sentence anagrams: Eleven plus two") {
    val sentence = List( "eleven", "plus", "two" )
    val anas = sentenceAnagrams( sentence )
    assert( anas.length == 50526 )
    assert( anas.contains( List("twelve", "plus", "one" ) ) )
  }
}