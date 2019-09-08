package ScalaPostFixLanguage

object PostFixApp extends App {
	assert(PostFixInterpreter.run("0") == "0")
	assert(PostFixInterpreter.run("0 1") == "1")
	assert(PostFixInterpreter.run("0 1 2 pop") == "1")
	assert(PostFixInterpreter.run("0 1 2 pop pop") == "0")
	assert(PostFixInterpreter.run("1 2 swap 3 pop") == "1")

}

/*


 */
