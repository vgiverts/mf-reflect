package com.marsfog.reflect

import java.util.concurrent.ConcurrentHashMap
import scala.collection.JavaConversions._
import java.lang.{Class, String}
import collection.{mutable => mut}
import java.lang.reflect._
import scala.Array

/**
 * Created by IntelliJ IDEA.
 * User: vgiverts
 * Date: 5/10/11
 * Time: 11:23 AM
 */

object ReflectionManager extends Reflect {
  val TYPE_FIELD = "_t"
  val mapClass = classOf[Map[_, _]]
  val listClass = classOf[Iterable[_]]
  val classToClassInfo: mut.ConcurrentMap[Class[_], ClassInfo[_]] = new ConcurrentHashMap[Class[_], ClassInfo[_]](1024, 0.5f, 32)
  val classNameToClassInfo: mut.ConcurrentMap[String, ClassInfo[_]] = new ConcurrentHashMap[String, ClassInfo[_]](1024, 0.5f, 32)


  def getCause(e: Throwable): Throwable = getCause(e, new mut.HashSet[Throwable])

  def getCause(e: Throwable, seenExceptions: mut.HashSet[Throwable]): Throwable =
    if (e.getCause == null || seenExceptions.contains(e)) e
    else {
      seenExceptions.add(e)
      getCause(e, seenExceptions)
    }

  def typeToGenericParameters(t: Type): Seq[ClassInfo[_]] = t match {
    case p: ParameterizedType => p.getActualTypeArguments.map(
      _ match {
        case clazz: Class[_] => getClassInfo(clazz)
        case typeParam: ParameterizedType => {
          new ClassInfoImpl(typeParam.getRawType.asInstanceOf[Class[_]], typeToGenericParameters(typeParam))
        }
      }
    )
    case _ => List()
  }

  def getClassInfo[T](clazz: Class[T]): ClassInfo[T] = classToClassInfo.getOrElseUpdate(clazz, new ClassInfoImpl[T](clazz, Nil)).asInstanceOf[ClassInfo[T]]

  def getClassInfo[T](className: String): ClassInfo[T] = classNameToClassInfo.getOrElseUpdate(className, getClassInfo(Class.forName(className))).asInstanceOf[ClassInfo[T]]

  def convertToTypes(args: Seq[_], types: Seq[ClassInfo[_]]): Seq[AnyRef] =
    types.zip(args).map(z => z._1.convertToInstance(z._2.asInstanceOf[AnyRef]).asInstanceOf[AnyRef])


}


class RealMethod(val method: Method) extends MethodInfo {
  def getName = method.getName

  val getParameterTypes: Seq[ClassInfo[_]] =
    method.getParameterTypes.zip(method.getGenericParameterTypes).map(p => new ClassInfoImpl(p._1, ReflectionManager.typeToGenericParameters(p._2)))

  // Auto-convert string arguments to the expected types of the parameters
  def invoke(obj: AnyRef, args: AnyRef*): AnyRef =
    method.invoke(obj, ReflectionManager.convertToTypes(args, getParameterTypes): _*)

  def isPublic = Modifier.isPublic(method.getModifiers)
}


class RealField[T](val field: Field, val declaringClass:ClassInfo[_]) extends FieldInfo[T] {

  def getName = field.getName

  def get(obj: AnyRef) = field.get(obj).asInstanceOf[T]

  def set(obj: AnyRef, value: T) = {
//    println("setting field: " + field.getDeclaringClass.getName + "." + getName)
    field.set(obj, value)
  }

  val getType = new ClassInfoImpl[T](field.getType.asInstanceOf[Class[T]], ReflectionManager.typeToGenericParameters(field.getGenericType))
}

class TypeField(classInfo: ClassInfo[_], val declaringClass:ClassInfo[_]) extends FieldInfo[AnyRef] {

  val getType: ClassInfo[AnyRef] = ReflectionManager.getClassInfo(classOf[Class[_]]).asInstanceOf[ClassInfo[AnyRef]]

  def get(obj: AnyRef) = classInfo.shortClassName

  def getName = ReflectionManager.TYPE_FIELD

  def set(obj: AnyRef, value: AnyRef) {}

  override def convertAndSet(obj: AnyRef, value: AnyRef) {}
}

object PrimitiveValue {
  def unapply(value: Any) = {
    if (value != null && ReflectionManager.getClassInfo(value.asInstanceOf[Object].getClass).isPrimitive)
      Some(value)
    else
      None
  }
}

object StringList {
  def unapply(stringList: List[_]): Option[List[(AnyRef, AnyRef)]] =
    if (stringList.length == 0 || stringList.last.isInstanceOf[Tuple2[_, _]])
      Some(stringList.asInstanceOf[List[(AnyRef, AnyRef)]])
    else
      None
}

object StringMap {
  def unapply(stringMap: Map[_, _]): Option[Map[String, AnyRef]] =
    if (stringMap.lastOption.map(l => l._1.isInstanceOf[String]).getOrElse(false))
      Some(stringMap.asInstanceOf[Map[String, AnyRef]])
    else
      None
}