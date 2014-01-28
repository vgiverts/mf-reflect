package com.marsfog.reflect

import java.lang.reflect.Constructor

/**
 * Created by IntelliJ IDEA.
 * User: vgiverts
 * Date: 9/25/11
 * Time: 10:08 PM
 */

trait ClassInfo[T] {

  def methods: Seq[MethodInfo]

  def fields: Seq[FieldInfo[_]]

  def isPrimitive: Boolean

  def isInstance(value: AnyRef): Boolean

  def getField(fieldName: String): Option[FieldInfo[_]]

  def getMethods(methodName: String): Option[Seq[MethodInfo]]

  def newInstance(args: AnyRef*): T

  def newInstanceFromString(value: String): T

  def getDefaultValue: Any

  def shortClassName: String

  def clazz: Class[T]

  def genericParams: Seq[ClassInfo[_]]

  def convertToInstance(value: AnyRef): T

}