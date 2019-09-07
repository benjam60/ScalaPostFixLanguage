package ScalaPostFixLanguage

object PostFixApp extends App {
	assert(PostFixInterpreter.run("0 0 add") == "0")
	assert(PostFixInterpreter.run("0 1 add") == "1")
	assert(PostFixInterpreter.run("1 1 add") == "2")
}

/*


 */
