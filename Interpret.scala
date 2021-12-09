/*
 * This file is part of COMP3000 assignment 1.
 *
 * Copyright (C) 2021 Kym Haines, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

// https://www.scala-lang.org/api/2.12.8/scala/util/matching/Regex.html
// https://www.scala-lang.org/api/2.12.8/scala/collection/immutable/List.html
// https://www.scala-lang.org/api/2.12.8/scala/collection/immutable/StringLike.html
// https://www.scala-lang.org/api/2.12.8/scala/collection/immutable/Map.html

//Runde Jia
//44434065

package org.mq.interpret

import scala.util.matching._

object Interpret {

  sealed abstract class LObject
  case class LSymbol(sym:String) extends LObject
  case class LNumber(num:Int) extends LObject
  case class LList(head:LObject, tail:LObject) extends LObject

  val nil = LSymbol("nil")
  val T = LSymbol("t")
  val error = LSymbol("ERROR")

  def resetEnv:Unit =
  {
    // TO DO: reset your functions/variables environment
  }

  // val pat = "TO DO: recognise all the language tokens".r
   val pat = "[(]|[)]|[-]?[0-9]+|[-+*/]|[a-z]([0-9]|[a-z])*".r

  def matchPat(line:String):List[String] =
                              pat.findAllIn(line.toLowerCase).toList

  // excludes brackets
  def strToLObj(s:String):LObject = 
  {
    // TO DO: convert a string token to an LObject; or error
    
    val ch = s.head                                                             //get head of the string, to test whether it is a LSymbol or LNumber
    if(ch.isLetter) LSymbol(s)
    else if(ch.isDigit) LNumber(s.toInt)
    else if(s == "+") LSymbol(s)                                                //Test if it is an operator
    else if(s == "-") LSymbol(s)
    else if((ch == '-') && (s.charAt(1).isDigit)) LNumber(s.toInt)             //negative number
    else if(s == "*") LSymbol(s)
    else if(s == "/") LSymbol(s)
    else error
  }

  def tokensToLObjs(a:List[String]):Option[LObject] =
  {
    // TO DO: convert a list of token strings to an LObject
    // NOTE: anywhere () is seen, it should be interpreted as nil
    
    if(a.size == 0) None                                                    
    else if(a.size == 1) Some(strToLObj(a.head))                                  //handle a single token
    else if(a.toString() == "List((, ))") Some(nil)                               //handle (,)
    else {
      None
    }
    // case _ if a.toString() == "List((, ))"  => Some(nil)
    // case _ if a.size == 1 => Some(strToLObj(a.head)) 
    // case head :: tl => LList.
    // case _ => None
  }

  // for testing
  def lineToLObj(line:String):LObject = tokensToLObjs(matchPat(line)) match
  {
  case Some(s) => s
  case None    => error
  }

  def setValue(varName:String, value:LObject):Unit =
  {
    // TO DO: assign a value to a variable
    
  }

  def getValue(varName:String):LObject =
  {
    // TO DO: get the value of a variable; or error if variable not defined
    error
  }

  def add(a:LObject, b:LObject):LObject = (a,b) match{              //Using match/cases, because it is effecient
    case (LNumber(s), LNumber(d)) => LNumber(s + d)                 //s represents value of a, d represents value of b. if the case matches, add two LNumber
    case _ => error
  }    // TO DO

  def sub(a:LObject, b:LObject):LObject = (a,b) match{
    case (LNumber(s), LNumber(d)) => LNumber(s - d)
    case _ => error
  }     // TO DO

  def mul(a:LObject, b:LObject):LObject = (a,b) match{
    case (LNumber(s), LNumber(d)) => LNumber(s * d)
    case _ => error
  }    // TO DO

  def div(a:LObject, b:LObject):LObject = (a,b) match{
    case (LNumber(s), LNumber(d)) => LNumber(s / d)
    case _ => error
  }     // TO DO

  def car(a:LObject):LObject = a match{                          //get first LObject
    case l:LList => l.head                                      //if a is a list
    case s:LSymbol => error                                     //other LObject gives error
    case n:LNumber => error
  }       // TO DO

  def cdr(a:LObject):LObject = a match{                         //get last LObject
    case l:LList => l.tail
    case s:LSymbol => error
    case n:LNumber => error
  }          // TO DO

  def cons(a:LObject, b:LObject):LObject = {
    LList(a,b)                                                   //simply construct two LObjects to a list
  }   // TO DO

  def eeqq(a:LObject, b:LObject):LObject = {
    if(a == b) return T                                         //if a and b are equal, return T, otherwise return nil
    else return nil 
  }   // TO DO

  def setq(v:String, b:LObject):LObject = error
     // TO DO

  def iiff(cond:LObject, ifThen:LObject, ifElse:LObject):LObject = cond match{
    case T => eval(ifThen)                                  //if condition is T, go to evaluate ifThen expression, otherwise ifElse expression
    case `nil` => eval(ifElse)
    case _ => error
  } //TO DO

  def defun(name:String, arg:String, body:LObject):LObject =
  {
    // TO DO: define a function
    // the function definition source would look like:
    //      (def name (arg) body)
    LSymbol(name)
  }

  def funCall(name:String, arg:LObject):LObject = error // TO DO

  def eval(a:LObject):LObject = a match
  {
  case LSymbol("nil") => nil
  case LSymbol("t") => T
  // TO DO: add cases for all the other possibilities in the eval table
  //        in the spec
  case LNumber(num) => LNumber(num)                 //handle single LNumber
  case LSymbol(sym) => LSymbol(sym)                 //handle single LSymbol
  case LList(LSymbol("quote"), LList(m, nil)) => m          //handle quote, by just take head LObjects
  case LList(LSymbol("if"), LList(b,LList(c,LList(d, nil)))) => iiff(eval(b),eval(c),eval(d))             //evaluate if
  case LList(LSymbol(k),LList((b),LList(c,nil))) if(a.toString.contains("LList(LList(")) => k match {        //if there are nested LLists, go to next cases
    case "+" => add(eval(b),eval(c))                              //evaluate inside LLists first, then come to add
    case "-" => sub(eval(b),eval(c))                              //same as above
    case "*" => mul(eval(b),eval(c))
    case "/" => div(eval(b),eval(c))
    case "cons" => cons(eval(b), eval(c))
    case "eq" => eeqq(eval(b),eval(c))
  }
  case LList(LSymbol("+"),LList((b),LList(c,nil))) => add(b,c)        //simply add two LObjects
  case LList(LSymbol("-"),LList((b),LList(c,nil))) => sub(b,c)        //same as above
  case LList(LSymbol("*"),LList((b),LList(c,nil))) => mul(b,c)
  case LList(LSymbol("/"),LList((b),LList(c,nil))) => div(b,c)
  case LList(LSymbol("car"),LList(b,c)) => car(eval(b))             //evaluate case first, then get the first element
  case LList(LSymbol("cdr"),LList(b,c)) => cdr(eval(b))             //same as above
  case LList(LSymbol("eq"), LList(b,LList(c, nil))) => eeqq(b,c)    //Determine if equal
  case LList(LSymbol("eval"), LList(b, nil)) => eval(b)             //eval after eval
  case _          => error
  }

  def showLine(s:LObject):Unit = { show(s);  println() }

  def show(s:LObject):Unit = s match
  {
  case LList(h, t)  => print("(")
                       show(h)
                       showList(t)
                       print(")")
  case LSymbol(a)   => print(a)
  case LNumber(a)   => print(a)
  }

  def showList(s:LObject):Unit = s match
  {
  case LSymbol("nil") =>
  case a:LList => print(" ")
                  show(a.head)
                  a.tail match
                  {
                  case b:LList => showList(b)
                  case _ =>
                  }
  case _ => print(" . ")
            show(s)
  }

}
