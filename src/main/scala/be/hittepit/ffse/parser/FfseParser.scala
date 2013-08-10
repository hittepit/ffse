package be.hittepit.ffse.parser

import scala.util.parsing.combinator.JavaTokenParsers

import be.hittepit.ffse.model.Command
import be.hittepit.ffse.model.Engine
import be.hittepit.ffse.model.Event
import be.hittepit.ffse.model.State
import be.hittepit.ffse.model.StateType
import be.hittepit.ffse.model.Transition

/*
name ::= [a-zA-Z0-9]+

className ::= [a-zA-Z0-9]+

events ::= "events" <name>* "end"

commands ::= "commands" <command>* "end"

command ::= <name> => <className>

states ::= <startState> <endState>* <state>* 

stateBody ::= [<actions>] <transition> <transition>*

actions ::= "actions" "{" <name>* "}"

transition ::= <name> "=>" <name>

startState ::= "start" <name> <stateBody> "end"

endState ::= "finish" <name> [<actions>] "end"

state ::= "state" <name> <stateBody> "end"

engine ::= "engine" <name> [<events>] [<commands>] [<states>] "end"

engines ::= <engine>*

*/


object FfseParser extends JavaTokenParsers {
	val name = """\b(?!end)[a-zA-Z0-9]+""".r
	val className = """\b(?!end)\b[a-zA-Z][\.a-zA-Z0-9]+""".r
	  
	def commands:Parser[List[Command]] = "commands"~>rep(command)<~"end"
	
	def command:Parser[Command] = name~"=>"~className ^^ {case n~"=>"~c => Command(n,c)}
	
	def events:Parser[List[Event]] = "events"~>rep(name)<~"end" ^^ {case ns => ns.map(Event(_))}
	
	def transition:Parser[Transition] = name~"=>"~name ^^ {case en~"=>"~sn => Transition(en,sn)}
	
	def actions:Parser[List[String]] = "actions"~"{"~rep(name)~"}" ^^ {case "actions"~"{"~ans~"}" => ans}
	
	def stateBody:Parser[(List[String],List[Transition])] = opt(actions)~transition~rep(transition) ^^ {
	  case Some(ans)~t~ts => (ans,t::ts)
	  case None~t~ts => (Nil,t::ts)
	}
	
	def startState:Parser[State] = "start"~>name~stateBody<~"end" ^^ {case n~sb => State(n,sb._1,sb._2,StateType.START)}
	
	def endState:Parser[State] = "finish"~name~opt(actions)~"end" ^^ {
	  case "finish"~n~Some(as)~"end" => State(n,as,Nil,StateType.END)
	  case "finish"~n~None~"end" => State(n,Nil,Nil,StateType.END)
	}
	
	def state:Parser[State] = "state"~name~stateBody~"end" ^^ {case "state"~n~sb~"end" => State(n,sb._1,sb._2,StateType.STATE)}
	
	def engine:Parser[Engine] = "engine"~>name~events~opt(commands)~startState~rep(state|endState)<~"end" ^^ {
	  case n~es~Some(cs)~ss~sts => Engine(n,es,cs,ss,sts)
	  case n~es~None~ss~sts => Engine(n,es,Nil,ss,sts)
	}
	
	def engines:Parser[List[Engine]] = rep(engine) ^^ {case l => l}
}
