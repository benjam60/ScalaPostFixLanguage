package ScalaPostFixLanguage

object PostFixApp extends App {
	assert(PostFixInterpreter.run("(postfix (0) exec)") == "0")
	assert(PostFixInterpreter.run("(postfix ( 0 ) exec)") == "0")
	assert(PostFixInterpreter.run("(postfix 0 1)") == "1")
	assert(PostFixInterpreter.run("(postfix 0 1 2 pop)") == "1")
	assert(PostFixInterpreter.run("(postfix 0 1 2 pop pop)") == "0")
	assert(PostFixInterpreter.run("(postfix 1 2 swap 3 pop)") == "1")
	assert(PostFixInterpreter.run("(postfix (1) (2) swap exec pop)") == "2")
//	assert(PostFixInterpreter.run("(postfix (1 2) (3 4) swap exec pop)") == "1")

}

/*


 */
