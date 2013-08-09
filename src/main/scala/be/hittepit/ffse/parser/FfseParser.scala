package be.hittepit.ffse.parser

import scala.util.parsing.combinator.JavaTokenParsers
import be.hittepit.ffse.model.Machine
import be.hittepit.ffse.model.Command

/*
machines ::= <machine>*

machine ::= "machine" <name> [<events>] [<commands>] [<states>] "end"

name ::= [a-zA-Z0-9]+

className ::= [a-zA-Z0-9]+

events ::= "events" <name>* "end"

commands ::= "commands" <command>* "end"

command ::= <name> => <className>

states ::= <startState> <endState>* <state>* 

stateBody ::= [<actions>] <transition> <transition>*

actions ::= "actions" "{" <name>* "}"

transition ::= <name> "=>" <name>

startState ::= "start" <stateBody> "end"

endState ::= "finish" <stateBody> "end"

state ::= "state" <stateBody> "end"

*/

object FfseParser extends JavaTokenParsers {
	val name = """[a-zA-Z0-9]+""".r
	val className = """[a-zA-Z][\.a-zA-Z0-9]+""".r
	  
	def command:Parser[Command] = name~"=>"~className ^^ {case n~"=>"~c => Command(n,c)}
	
	def machines:Parser[List[Machine]] = rep(machine) ^^ {case l => l}
	
	def machine:Parser[Machine] = "machine"~name~"end" ^^ {case "machine"~n~"end" => new Machine(n)}
}
