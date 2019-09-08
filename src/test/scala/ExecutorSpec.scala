package ScalaPostFixLanguage

import org.scalatest.{FreeSpec, Matchers}

class ExecutorSpec extends FreeSpec with Matchers {

	"Executes add correctly" in {
		val programText = "(postfix 1 1 add)"
		PostFixInterpreter.run(programText) shouldBe 2
	}

	"Executes swap correctly" in {
		val programText = "(postfix 3 5 swap)"
		PostFixInterpreter.run(programText) shouldBe 3
	}

}
