/**
 * Created with IntelliJ IDEA.
 * User: r.tabulov
 * Date: 05.07.13
 * Time: 13:02
 * To change this template use File | Settings | File Templates.
 */

import scala.util.parsing.combinator.JavaTokenParsers

class LoopParser extends JavaTokenParsers {
  override type Elem = Char

  def keywords: Parser[Any] = "begin" | "end" | "for" | "do"

  def identifier = not(keywords) ~ ident

  def integer = wholeNumber ^^ {
    _.toInt
  }

  //  def loop =
  //    "for" ~ identifier ~ ":=" ~ integer ~ "to" ~ integer ~ "do" ~ block ^^ {
  //      case f ~ variable ~ i ~ lBound ~ t ~ uBound ~ b ~ statement => ForLoop(variable, lBound, uBound, statement)
  //    }

  //def statements = statement*
  //  def block = "begin" ~> statements <~ "end" ^^ {
  //    l => Block(l)
  //  }


  //pascal ebnf
  def compoundStatement: Parser[Any] = "begin" ~ opt(statements) ~ "end"

  def statements: Parser[Any] = rep(statement ~ ";")

  def statement: Parser[Any] = label ~ ":" ~ unlabelledStatement | unlabelledStatement

  def label: Parser[Any] = unsignedInteger

  def unsignedInteger: Parser[Any] = NUM_INT

  def unsignedNumber: Parser[Any] = unsignedInteger | unsignedReal

  def unsignedReal: Parser[Any] = NUM_INT

  def NUM_INT: Parser[Any] = floatingPointNumber

  def unlabelledStatement: Parser[Any] = simpleStatement | structuredStatement

  def structuredStatement: Parser[Any] = compoundStatement | conditionalStatement | repetetiveStatement | withStatement

  def conditionalStatement: Parser[Any] = ifStatement | caseStatement

  def ifStatement: Parser[Any] = "if" ~ expression ~ "then" ~ statement ~ opt("else" ~ statement)

  def caseStatement: Parser[Any] = "case" ~ expression ~ "of" ~ caseListElement ~ rep(";" ~ caseListElement) ~ ((";" ~ "else" ~ statements) ?) ~ "end"

  def caseListElement: Parser[Any] = constList ~ ":" ~ statement

  def constList: Parser[Any] = constant ~ rep("," ~ constant)

  def constantDefinitionPart: Parser[Any] = "const" ~ constantDefinition ~ rep(";" ~ constantDefinition) ~ ";"

  def constantDefinition: Parser[Any] = identifier ~ "=" ~ constant

  def constantChr: Parser[Any] = "chr" ~ "(" ~ unsignedInteger ~ ")"

  def constant: Parser[Any] = unsignedNumber | identifier | string | constantChr

  def string: Parser[Any] = stringLiteral

  def repetetiveStatement: Parser[Any] = whileStatement | repeatStatement | forStatement

  def whileStatement: Parser[Any] = "while" ~ expression ~ "do" ~ statement

  def repeatStatement: Parser[Any] = "repeat" ~ statements ~ "until" ~ expression

  def forStatement: Parser[Any] = "for" ~ identifier ~ ":=" ~ forList ~ "do" ~ statement

  def forList: Parser[Any] = initialValue ~ ("to" | "downto") ~ finalValue

  def initialValue: Parser[Any] = expression

  def finalValue: Parser[Any] = expression

  def withStatement: Parser[Any] = "with" ~ recordVariableList ~ "do" ~ statement

  def recordVariableList: Parser[Any] = variable ~ rep("," ~ variable)

  def simpleStatement: Parser[Any] = assignmentStatement | procedureStatement | gotoStatement | emptyStatement

  def emptyStatement: Parser[Any] = """[(\r?\n)\s]+""".r

  def procedureStatement: Parser[Any] = identifier ~ opt("(" ~ parameterList ~ ")")

  def parameterList: Parser[Any] = actualParameter ~ rep("," ~ actualParameter)

  def actualParameter: Parser[Any] = expression

  def gotoStatement: Parser[Any] = "goto" ~ label

  def assignmentStatement: Parser[Any] = variable ~ ":=" ~ expression

  /** A variable is an id with a suffix and can look like:
    * id
    * id[expr,...]
    * id.id
    * id.id[expr,...]
    * id^
    * id^.id
    * id.id[expr,...]
    * ...
    *
    * LL has a really hard time with this construct as it's naturally
    * left-recursive.  We have to turn into a simple loop rather than
    * recursive loop, hence, the suffixes.  I keep in the same rule
    * for easy tree construction.
    */
  def variable: Parser[Any] = ("@" ~ identifier // AT is root of identifier; then other op becomes root
    | identifier
    ) ~ rep("[" ~ expression ~ rep("," ~ expression) ~ "]" | "(." ~ expression ~ rep("," ~ expression) ~ ".)" | "." ~ identifier | "^")

  def expression: Parser[Any] = simpleExpression ~ rep(("=" | "<>" | "<" | "<=" | ">=" | ">" | "in") ~ simpleExpression)

  def simpleExpression: Parser[Any] = term ~ rep(("+" | "-" | "or") ~ term)

  def term: Parser[Any] = signedFactor ~ rep(("*" | "/" | "div" | "mod" | "and") ~ signedFactor)

  def signedFactor: Parser[Any] = opt("+" | "-") ~ factor

  def factor: Parser[Any] = variable | "(" ~ expression ~ ")" | functionDesignator | unsignedConstant | set | "not" ~ factor

  def set: Parser[Any] = "[" ~ elementList ~ "]" | "(." ~ elementList ~ ".)"

  def elementList: Parser[Any] = element ~ rep("," ~ element)

  def element: Parser[Any] = expression ~ opt(".." ~ expression)

  def unsignedConstant: Parser[Any] = unsignedNumber | constantChr | string | "nil"

  def functionDesignator: Parser[Any] = identifier ~ "(" ~ parameterList ~ ")"
}

abstract trait Statement

case class Block(statements: List[Statement]) extends Statement

case class ProcCall(name: String, args: List[Statement]) extends Statement

case class ForLoop(variable: String, lowerBound: Int, upperBound: Int, statement: Statement) extends Statement

object TestLoopParser extends LoopParser with App {
  //val cmd = """i := 0;"""
  // val cmd = """showMessage(avAttr[i])"""
  //  val cmd = """begin end;"""
  val cmd = """begin for i := 0 to 10 do begin showMessage(avAttr[i]); end;end;"""
  parseAll(statements,
    cmd.stripMargin) match {
    case Success(lup, _) => println(lup)
    case x => println(x)
  }
}
