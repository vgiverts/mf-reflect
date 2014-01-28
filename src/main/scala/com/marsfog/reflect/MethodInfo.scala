package com.marsfog.reflect

import java.util.concurrent.ConcurrentHashMap
import scala.collection.JavaConversions._
import java.lang.{Class, String}
import collection.{mutable => mut}
import java.lang.reflect._

/**
 * Created by IntelliJ IDEA.
 * User: vgiverts
 * Date: 5/10/11
 * Time: 11:23 AM
 */

trait MethodInfo {

  def isPublic: Boolean

  def getName: String

  def getParameterTypes: Seq[ClassInfo[_]]

  def invoke(obj: AnyRef, args: AnyRef*): AnyRef
}

















