package ScalaPostFixLanguage

import ScalaPostFixLanguage.PostFixInterpreter._
import org.scalatest.{FreeSpec, Matchers}

class PostFixInterpreterSpec extends FreeSpec with Matchers {

	"Parses correctly" in {
		val programText = "(postfix 1 1 add)"
		PostFixInterpreter.parse(programText) shouldBe List(ParsedInt(1), ParsedInt(1), ParsedCommand("add"))
	}

}
