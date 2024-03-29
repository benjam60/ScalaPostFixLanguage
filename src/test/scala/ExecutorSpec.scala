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

	"Executes only stack pushes correctly" in {
		val programText = "(postfix 1 2 3 4)"
		PostFixInterpreter.run(programText) shouldBe 4
	}

	"Executes nested commands" in {
		val programText = "(postfix (1 2 add) exec 3 add)"
		PostFixInterpreter.run(programText) shouldBe 6
	}

	"Executes nested commands more" in {
		val programText = "(postfix 3 (1 2 add) exec add)"
		PostFixInterpreter.run(programText) shouldBe 6
	}


	"Executes all nested args more" in {
		val programText = "(postfix (2 2 add) exec (1 2 add) exec add)"
		PostFixInterpreter.run(programText) shouldBe 7
	}

	"Complex ops" in {
		val programText = "(postfix 1 (2) exec (3) exec swap (4 3 add) exec add)"
		PostFixInterpreter.run(programText) shouldBe 9
	}

	"Complex ops with others" in {
		val programText = "(postfix 4 2 sub 2 mul)"
		PostFixInterpreter.run(programText) shouldBe 4
	}

	"nget example 1" in {
		val programText = "(postfix 4 5 1 nget)"
		PostFixInterpreter.run(programText) shouldBe 4
	}

	"nget example 2" in {
		val programText = "(postfix 4 5 2 nget)"
		PostFixInterpreter.run(programText) shouldBe 5
	}

	"Squaring program" in {
		val programText = "(postfix 8 1 nget mul)"
		PostFixInterpreter.run(programText) shouldBe 64
	}

	"Squaring take previous stack" in {
		val programText = "(postfix 2 (2 mul) exec)"
		PostFixInterpreter.run(programText) shouldBe 4
	}

}
