package com.marsfog.reflect

/**
 * Created by IntelliJ IDEA.
 * User: vgiverts
 * Date: 9/25/11
 * Time: 10:01 PM
 */

trait Reflect {

  def getField(obj: AnyRef, fieldName: String): Option[FieldInfo[_]] = getField(obj.asInstanceOf[Object].getClass, fieldName)

  def getField(clazz: Class[_], fieldName: String): Option[FieldInfo[_]] = getClassInfo(clazz).getField(fieldName)

  def getFields(clazz: Class[_]): Seq[FieldInfo[_]] = getClassInfo(clazz).fields

  def getFields(obj: AnyRef): Seq[FieldInfo[_]] = getFields(obj.asInstanceOf[Object].getClass)

  def getMethods(obj: AnyRef, methodName: String): Seq[MethodInfo] = getClassInfo(obj).getMethods(methodName).getOrElse(List())

  def getMethods(clazz: Class[_], methodName: String): Seq[MethodInfo] = getClassInfo(clazz).getMethods(methodName).getOrElse(List())

  def getAllMethods(clazz: Class[_]): Seq[MethodInfo] = getClassInfo(clazz).methods

  def getAllMethods(obj: AnyRef): Seq[MethodInfo] = getClassInfo(obj).methods

  def newInstance[T](className: String, args: AnyRef*): T = getClassInfo(className).newInstance(args: _*)

  def newInstance[T](clazz: Class[T], args: AnyRef*): T = getClassInfo(clazz).newInstance(args: _*)

  def getClassInfo[T <: Object](obj: T): ClassInfo[T] = getClassInfo(obj.getClass.asInstanceOf[Class[T]])

  def getClassInfo[T](clazz: Class[T]): ClassInfo[T]

  def getClassInfo[T](className: String): ClassInfo[T]

}