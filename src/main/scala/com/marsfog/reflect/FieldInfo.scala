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

trait FieldInfo[T] {

  def getName: String

  def get(obj: AnyRef): T

  def set(obj: AnyRef, value: T)

  def convertAndSet(obj: AnyRef, value: AnyRef) = {
//    println("about to set field: " + declaringClass.clazz.getName + "." + getName)
    set(obj, getType.convertToInstance(value))
  }

  def declaringClass: ClassInfo[_]

  def getType: ClassInfo[T]
}

















