package ScalaPostFixLanguage

object PostFixInterpreter {

	sealed trait ParsedValue
	case class ParsedInt(get : Int) extends ParsedValue
	case class ParsedCommand(get : String) extends ParsedValue
	case class ParsedCommandSequence(get : String) extends ParsedValue

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
		val topLevelCmdsAndSeq = breakUp(removeHeader)
		topLevelCmdsAndSeq.flatMap { elt =>
			if (elt(0) == '(')
				List(ParsedCommandSequence(elt.drop(1).dropRight(1)))
			else
				parseWithoutCommandSequence(elt)  }
	}

	private def parseWithoutCommandSequence(input : String) =
		input.trim.split("\\s+").map { elt =>
			if (elt(0).isDigit) ParsedInt(elt(0).asDigit) else ParsedCommand(elt) }.toList

	def breakUp(input : String) : List[String] = {
		val maybeOpenParensIndex = input.indexOf('(')
		maybeOpenParensIndex match {
			case openIndex if openIndex == 0 =>
				val closingIndex = getClosingParenethesisIndex(input)
				val (executableSequence, rest) = input.splitAt(closingIndex + 1)
				List(executableSequence, rest)
			case openIndex if openIndex > 0 => throw new RuntimeException("not implemented yet")
			case -1 => List(input)
		}
	}

	def getClosingParenethesisIndex(input : String) : Int = {

		def traverse(input : String, currIndex : Int, numOpen : Int, numClosed : Int) : Int = {
			if (numOpen == numClosed) { currIndex }
			else {
				val nextChar = input(currIndex + 1)
				if (nextChar == '(') traverse(input.drop(0), currIndex + 1, numOpen + 1, numClosed)
				else if (nextChar == ')') traverse(input.drop(0), currIndex + 1, numOpen, numClosed + 1)
				else traverse(input.drop(0), currIndex + 1, numOpen, numClosed)
			}
		}

		traverse(input.drop(0), currIndex = 0, numOpen = 1, numClosed = 0)

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