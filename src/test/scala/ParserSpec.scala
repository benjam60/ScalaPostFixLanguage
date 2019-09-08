package ScalaPostFixLanguage

import ScalaPostFixLanguage.PostFixInterpreter._
import org.scalatest.{FreeSpec, Matchers}

class ParserSpec extends FreeSpec with Matchers {

	"Parses basic add correctly" in {
		val programText = "(postfix 1 1 add)"
		PostFixInterpreter.parse(programText) shouldBe List(ParsedInt(1), ParsedInt(1), ParsedCommand("add"))
	}

	"Parses executable sequence correctly" in {
		val programText = "(postfix (1) 1 add)"
		PostFixInterpreter.parse(programText) shouldBe
			List(ParsedCommandSequence("1"), ParsedInt(1), ParsedCommand("add"))
	}

	"Parses bigger executable sequence correctly" in {
		val programText = "(postfix (1 1 add) 1 add)"
		PostFixInterpreter.parse(programText) shouldBe
			List(ParsedCommandSequence("1 1 add"), ParsedInt(1), ParsedCommand("add"))
	}

	"Parses second executable sequence correctly" in {
		val programText = "(postfix 1 (1 1 add) add)"
		PostFixInterpreter.parse(programText) shouldBe
			List(ParsedInt(1), ParsedCommandSequence("1 1 add"), ParsedCommand("add"))
	}

	"Parses many nested things" in {
		val programText = "(postfix 1 ((1 1 add) (1) add) add)"
		PostFixInterpreter.parse(programText) shouldBe
			List(ParsedInt(1), ParsedCommandSequence("(1 1 add) (1) add"), ParsedCommand("add"))
	}

	"Parses all nested arguments" in {
		val programText = "(postfix (1) (2) add)"
		PostFixInterpreter.parse(programText) shouldBe
			List(ParsedCommandSequence("1"), ParsedCommandSequence("2"), ParsedCommand("add"))
	}

	"Parses later nested arguments" in {
		val programText = "(postfix 1 (2) (3) add)"
		PostFixInterpreter.parse(programText) shouldBe
			List(ParsedInt(1), ParsedCommandSequence("2"), ParsedCommandSequence("3"), ParsedCommand("add"))
	}
}
