import java.io.File

/**
 * Sentence anagrams generation
 * MSE course, T-AdvPrPa course
 */
package object anagrams {

	/**
	 * Loads a file dictionary which should be located in the src directory
	 */
	def loadDictionary() = {
		val wordstream = Option {
			getClass.getClassLoader.getResourceAsStream("resources/linuxwords.txt")
		} getOrElse {
			sys.error("Could not load word list, dictionary file not found")
		}
		try {
			val s = io.Source.fromInputStream(wordstream)
			s.getLines.toList
		} catch {
			case e: Exception ⇒
				println("Could not load word list: " + e)
				throw e
		} finally {
			wordstream.close()
		}
	}
}
