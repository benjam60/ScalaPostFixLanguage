package ScalaPostFixLanguage

object PostFixInterpreter {

	sealed trait ParsedValue
	case class ParsedInt(get : Int) extends ParsedValue
	case class ParsedCommand(get : String) extends ParsedValue
	case class CommandSequence(get : String) extends ParsedValue

	sealed trait StackValue
	case class IntValue(get : Int) extends StackValue
	case class ExecutableSequence(get : String) extends StackValue

	def run(program : String) : String = {
		val tokens = program.split("\\s+").toList
		val stack = List[StackValue]()
		executeTokens(tokens, stack)
	}

	def parse(program : String): Seq[ParsedValue] = {
		val removeHeader = program.drop(1).dropRight(1).replaceFirst("postfix ", "")
		val tokens: Seq[String] = removeHeader.split("\\s+").toList
		tokens.map { elt => if (elt(0).isDigit) ParsedInt(elt(0).asDigit) else ParsedCommand(elt) }
	}

	def executeTokens(tokens : List[String], stack : List[StackValue]): String =
		if (tokens.nonEmpty) {
			val element = tokens.head
			if (element(0).isDigit) executeTokens(tokens.tail, IntValue(element(0).asDigit) :: stack)
			else {
				(element, stack) match {
					case ("pop", _) => executeTokens(tokens.tail, stack.tail)
					case ("swap", x1::x2::xs) => executeTokens(tokens.tail, x2::x1::xs)
					case ("exec", _) => ""
					case _ => throw new RuntimeException("No known method or stack size is wrong")
				}
			}
		} else stack.head.toString

}
