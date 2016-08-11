package com.givorenon.evaluator

import Array._
import scala.language.implicitConversions

/** Класс Expression представляет обертку над деревьями выражений.
  */
class Expression private (root: Expression.Node) {
  import Expression._

  // Вычислить значение выражения, если возможно.
  def evaluate: Option[Int] = root.evaluate
}

/** Объект Expression содержит внутреннее представление дерева выражения и парсер.
  */
object Expression {
  /************************************************************************************************
    * Внутренняя структура дерева:                                                                 *
    ************************************************************************************************/

  // Абстрактный класс, представляющий вершину в дереве.
  private abstract class Node {
    // Реализации методов по умолчанию:
    def evaluate: Option[Int] = None
  }

  private case class Constant(value: Int) extends Node {
    override def evaluate = Some(value)
  }

  private case class Variable(name: String) extends Node {
  }

  private case class Function(name: String, argument: Node) extends Node {
    override def evaluate = derivate(argument.evaluate)
  }

  private case class Negate(expr: Node) extends Node {
    override def evaluate = expr.evaluate map (- _)
  }

  // Класс, представляющий все три оператора.
  private case class Operator(opType: Char, left: Node, right: Node) extends Node {
    override def evaluate = {
      // Выбираем каррированную функцию для подсчета результата.
      val func = opType match {
        case '+' => { x: Int => y: Int => x + y}
        case '*' => { x: Int => y: Int => x * y}
        case '/' => { x: Int => y: Int => x / y}
      }
      // Применяем ее к (может быть) подсчитанным результатам детей.
      left.evaluate map func flatMap (right.evaluate.map _)
    }
  }

  /************************************************************************************************
    * Парсер:                                                                                      *
    * Используется простой метод рекурсивного спуска (LL(1) грамматика) с некоторыми модификациями *
    * для обработки левоассоциативных операторов.                                                  *
    ************************************************************************************************/
  private case class Parser(str: String) {
    /**********************************************************************************************
      * Токенайзер:                                                                                *
      **********************************************************************************************/
    abstract class Token
    case class Number(value: Int) extends Token
    case class Id(value: String) extends Token
    case class Op(value: Char) extends Token
    case class Eol() extends Token

    // Текущая позиция в строке.
    var pos: Int = 0
    // Текущий токен.
    var currentToken: Token = _
    advance()

    // Пропустить пробелы.
    private def skipWhitespace() = {
      while (pos < str.length && str(pos) == ' ')
        pos += 1
    }

    // Считать следующий токен и обновить состояние.
    private def advance() =
      currentToken = nextToken()

    // Разобрать токен.
    private def nextToken(): Token = {
      // Пропускаем пробелы.
      skipWhitespace()
      // Если уперлись в конец, вернуть конец строки.
      if (pos >= str.length)
        return Eol()

      // Если уперлись в оператор, вернуть его.
      if ("()[]*/-+" contains str(pos)) {
        val res = Op(str(pos))
        pos += 1
        return res
      }

      // Если уперлись во что-то другое, значит это либо переменная, либо число.
      var numOrId = new StringBuilder
      // Пока строка содержит что-то похожее на переменную или число, акккумулируем это.
      while (pos < str.length && (str(pos).isDigit || str(pos).isLetter || str(pos) == '.')) {
        numOrId += str(pos)
        pos += 1
      }

      // Получаем накопленные символы.
      val res = numOrId.toString

      // Пытаемся определить, переменная ли это или число.
      if (res matches """[a-zA-Z]+""")
        return Id(res)
      if (res matches """[0-9]+(\.[0-9]+)?""")
        return Number(res.toInt)

      // Если ни то, ни другое, кидаем исключение.
      throw new Exception("Invalid token at position " + pos.toString)
    }

    /**********************************************************************************************
      * Грамматика:                                                                                *
      * Expression := Sum <Eol>                                                                    *
      * Sum := Product SumRest                                                                     *
      * SumRest := <+ -> Product SumRest                                                           *
      *         |  <e>                                                                             *
      * Product := Factor ProductRest                                                              *
      * ProductRest := </ *> Factor ProductRest                                                    *
      *             |  <e>                                                                         *
      * Factor := <-> Factor                                                                       *
      *        |  <(> Sum <)>                                                                      *
      *        |  FunctionOrVarible                                                                *
      *        |  <Number>                                                                         *
      * FunctionOrVarible := <Id> Argument                                                         *
      * Argument := <[> Sum <]>                                                                    *
      *           | <e>                                                                            *
      **********************************************************************************************/
    private def parseExpr(): Option[Node] =
      parseSum() flatMap { res => if (currentToken == Eol()) Some(res) else None }

    private def parseSum(): Option[Node] =
      parseProduct() flatMap { first => parseSumRest(first) }

    private def parseSumRest(left: Node): Option[Node] = currentToken match {
      case Op(op) => {
        if ("+-" contains op) {
          advance()
          parseProduct() flatMap { product =>
            val right = if (op == '-') Negate(product) else product
            parseSumRest(Operator('+', left, right))
          }
        } else {
          Some(left)
        }
      }
      case _ => Some(left)
    }

    private def parseProduct(): Option[Node] =
      parseFactor() flatMap { first => parseProductRest(first) }

    private def parseProductRest(left: Node): Option[Node] = currentToken match {
      case Op(op) => {
        if ("/*" contains op) {
          advance()
          parseFactor() flatMap { factor => parseProductRest(Operator(op, left, factor)) }
        } else {
          Some(left)
        }
      }
      case _ => Some(left)
    }

    private def parseFactor(): Option[Node] = currentToken match {
      case Op('-') => {
        advance()
        parseFactor map { Negate(_) }
      }
      case Op('(') => {
        advance()
        val res = parseSum()
        if (currentToken == Op(')')) {
          advance()
          res
        } else {
          None
        }
      }
      case _ => parseVar() orElse parseNumber()
    }

    private def parseNumber(): Option[Constant] = currentToken match {
      case Number(num) => {
        advance()
        Some(Constant(num))
      }
      case _ => None
    }

    private def parseArg(): Option[Node] = currentToken match {
      case Op('[') => {
        advance()
        val res = parseSum()
        if (currentToken == Op(']')) {
          advance()
          res
        } else {
          None
        }
      }
      case _ => None
    }

    private def parseVar(): Option[Node] = currentToken match {
      case Id(name) => {
        advance()
        parseArg() match {
          case Some(arg) => Some(Function(name, arg))
          case _ => Some(Variable(name))
        }
      }
      case _ => None
    }

    def parse() = parseExpr()
  }

  // Фабричный метод для Expression-ов.
  def apply(expr: String) = new Expression(Parser(expr).parse.get)

  var derivateArr = Array.fill[Int](10000001)(0);
  var lowestDivisor = Array.fill[Int](10000001)(0);
  var primes = Array.fill[Int](10000001)(-1);
  var pointer = 0;
  var firstCall = true;
  def derivate(n:Option[Int]) : Option[Int] = {
    if (firstCall) {
      firstCall = false
      derivateArr.update(0, 0);
      derivateArr.update(1, 0);
      for (i <- 2 to 10000000) {
        if (lowestDivisor(i) == 0) {
          lowestDivisor.update(i, i);
          derivateArr.update(i, 1);
          primes.update(pointer, i);
          pointer += 1;
        }
        var p = 0;
        while (primes(p) <= lowestDivisor(i) && primes(p) * i < 10000001 && primes(p) != -1) {
          lowestDivisor.update(i * primes(p), primes(p))
          derivateArr.update(i * primes(p), derivateArr(primes(p)) * i + derivateArr(i) * primes(p))
          p += 1;
        }
      }
    }
    if (n.isDefined)
      return Some(derivateArr(n.get));
    return None;
  }
}

object Helper {
  def evaluate(str: String): Unit = {
    val expr = Expression(str)
    expr.evaluate match {
      case Some(value) => println(str + " equals to " + value)
      case _ => println("Cannot evaluate expression because of unbound variables.")
    }
  }

  def solve(str: String, n: Int): scala.collection.mutable.Set[Int] = {
    var res = scala.collection.mutable.Set[Int]();
    for (i <- 1 to n) {
      val expr = Expression(str.replace("x", i.toString))
      expr.evaluate match {
        case Some(value) => {
          if (value == 0) {
            res += i;
          }
        }
        case _ => println("Cannot evaluate expression because of unbound variables.")
      }
    }
    return res;
  }

  def bruteCoefficients(str: String, n: Int, k: Int): Unit = {
    for (i <- 1 to k) {
      for (j <- 1 to k) {
        val anotherStr = str.replace("a", i.toString).replace("b", j.toString);
        val solutions = this.solve(anotherStr, n);
        if (solutions.size != n && solutions.size > 1) {
          print(anotherStr + " has solutions ")
          for (solution <- solutions)
            print(solution.toString + " ");
          println();
        }
      }
    }
  }

  def bruteCoefficient(str: String, n: Int, k: Int): Unit = {
    for (i <- 1 to k) {
      val anotherStr = str.replace("a", i.toString);
      val solutions = this.solve(anotherStr, n);
      if (solutions.size != n && solutions.size > 1) {
        print(anotherStr + " has solutions ")
        for (solution <- solutions)
          print(solution.toString + " ");
        println();
      }
    }
  }

}

object Main {
  def main(args: Array[String]) {
    //Helper.bruteCoefficient("d[d[x]] - x * 5 + a", 1000000, 10);
    Helper.solve("d[d[x]] - x * 5 + 1", 1000000)
  }
}
