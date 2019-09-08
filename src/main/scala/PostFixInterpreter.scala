package ScalaPostFixLanguage

object PostFixInterpreter {

	def run(program : String) : String = {
		val tokens = program.split("\\s+").toList
		val stack = List[Int]()
		executeTokens(tokens, stack)
	}

	def executeTokens(tokens : List[String], stack : List[Int]): String =
		if (tokens.nonEmpty) {
			val element = tokens.head
			if (element(0).isDigit) executeTokens(tokens.tail, element(0).asDigit :: stack)
			else {
				(element, stack) match {
					case ("pop", _) => executeTokens(tokens.tail, stack.tail)
					case ("swap", x1::x2::xs) => executeTokens(tokens.tail, x2::x1::xs)
					case _ => throw new RuntimeException("No known method or stack size is wrong")
				}
			}
		} else stack.head.toString

}
