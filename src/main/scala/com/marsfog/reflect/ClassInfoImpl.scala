package com.marsfog.reflect

import scala.collection.JavaConversions._
import java.lang.{Class, String}
import collection.{mutable => mut}
import java.lang.reflect.Constructor

/**
 * Created by IntelliJ IDEA.
 * User: vgiverts
 * Date: 5/10/11
 * Time: 11:23 AM
 */

object ClassInfoImpl {
  val classClass = classOf[Class[_]]
  val booleanClass = classOf[java.lang.Boolean]
  val integerClass = classOf[java.lang.Integer]
  val doubleClass = classOf[java.lang.Double]
  val floatClass = classOf[java.lang.Float]
  val longClass = classOf[java.lang.Long]
  val byteClass = classOf[java.lang.Byte]
  val shortClass = classOf[java.lang.Short]
  val characterClass = classOf[java.lang.Character]
  val stringClass = classOf[String]

  val defaultValues: Map[Class[_], Any] = Map(
    java.lang.Boolean.TYPE -> true,
    classOf[java.lang.Boolean] -> true,
    java.lang.Integer.TYPE -> 0,
    classOf[java.lang.Integer] -> 0,
    java.lang.Double.TYPE -> 0.0,
    classOf[java.lang.Double] -> 0.0,
    java.lang.Float.TYPE -> 0.0f,
    classOf[java.lang.Float] -> 0.0f,
    java.lang.Long.TYPE -> 0L,
    classOf[java.lang.Long] -> 0L,
    java.lang.Byte.TYPE -> 0.asInstanceOf[Byte],
    classOf[java.lang.Byte] -> 0.asInstanceOf[Byte],
    java.lang.Short.TYPE -> 0.asInstanceOf[Short],
    classOf[java.lang.Short] -> 0.asInstanceOf[Short],
    java.lang.Character.TYPE -> 0.asInstanceOf[Char],
    classOf[java.lang.Character] -> 0.asInstanceOf[Char]
  )

}

class ClassInfoImpl[T](val clazz: Class[T], val genericParams: Seq[ClassInfo[_]]) extends ClassInfo[T] {

//  println("ClassInfoImpl: " + clazz.getName)

  private val hierarchy: List[Class[_]] = getHierarchy(clazz)
  val shortClassName: String = clazz.getName.substring(if (clazz.getPackage != null) clazz.getPackage.getName.length + 1 else 0)

  private lazy val methodNameToMethodInfo: Map[String, Seq[MethodInfo]] = {
    val tmpMap = new mut.HashMap[String, mut.ListBuffer[MethodInfo]]
    hierarchy.map(clazz => clazz.getDeclaredMethods.map(m => {
      m.setAccessible(true)
      tmpMap.getOrElseUpdate(m.getName, new mut.ListBuffer[MethodInfo]()).append(new RealMethod(m))
    }))
    tmpMap.toMap
  }

  private lazy val fieldNameToFieldInfo: Map[String, FieldInfo[_]] =
    Map((ReflectionManager.TYPE_FIELD, new TypeField(ClassInfoImpl.this, ClassInfoImpl.this)) ::
      hierarchy.map(clazz => clazz.getDeclaredFields.map(f => {
        f.setAccessible(true)
        (f.getName, new RealField(f, this))
      })).flatten: _*)

  private lazy val annotationClassToAnnotation: Map[Class[_], AnyRef] = Map(clazz.getDeclaredAnnotations.map(a => (a.getClass, a)): _*)

  lazy val methods: Seq[MethodInfo] = methodNameToMethodInfo.values.flatten.toList
  lazy val fields: Seq[FieldInfo[_]] = fieldNameToFieldInfo.values.toList

  private def getHierarchy(clazz: Class[_]): List[Class[_]] =
    clazz.getSuperclass match {
      case null => List(clazz)
      case x => clazz :: getHierarchy(x)
    }

  val isPrimitive = ClassInfoImpl.defaultValues.containsKey(clazz)

  def isInstance(value: AnyRef): Boolean = clazz.isInstance(value)

  def getField(fieldName: String): Option[FieldInfo[_]] = fieldNameToFieldInfo.get(fieldName)

  def getMethods(methodName: String): Option[Seq[MethodInfo]] = methodNameToMethodInfo.get(methodName)

  def newInstance(args: AnyRef*): T = {
    val ctors = clazz.getConstructors.asInstanceOf[Array[Constructor[T]]]
    ctors.foldLeft[Option[T]](None)((newObj, ctor) => {
      newObj.orElse({
        val ctorArgs: Seq[AnyRef] =
          if (args.length > 0) args.asInstanceOf[Seq[AnyRef]]
          else ctor.getParameterTypes.map(t => ReflectionManager.getClassInfo(t).getDefaultValue.asInstanceOf[AnyRef])
        //todo: Instead of catching an exception, check to make sure that the params match the type. Also, try to convert them if necessary.
        if (ctorArgs.length == ctor.getParameterTypes.length) try {
          Some(ctor.newInstance(ctorArgs: _*))
        }
        catch {
          case e: Exception => None
        }
        else None
      })
    }).getOrElse(
      throw new RuntimeException("Failed to instantiate class: " + (clazz.getName + ", args: " + args))
    ).asInstanceOf[T]
  }

  val getDefaultValue: Any = ClassInfoImpl.defaultValues.getOrElse(clazz, null)


  def newInstanceFromString(value: String): T = {
    val v: Any = clazz match {
      case ClassInfoImpl.stringClass => value
      case ClassInfoImpl.classClass => Class.forName(value.asInstanceOf[String])
      case ClassInfoImpl.integerClass => java.lang.Integer.valueOf(value)
      case ClassInfoImpl.doubleClass => java.lang.Double.valueOf(value)
      case ClassInfoImpl.floatClass => java.lang.Float.valueOf(value)
      case ClassInfoImpl.longClass => java.lang.Long.valueOf(value)
      case ClassInfoImpl.byteClass => java.lang.Byte.valueOf(value)
      case ClassInfoImpl.shortClass => java.lang.Short.valueOf(value)
      case ClassInfoImpl.characterClass if (value.length == 1) => value.charAt(0)
      case java.lang.Integer.TYPE => java.lang.Integer.valueOf(value)
      case java.lang.Double.TYPE => java.lang.Double.valueOf(value)
      case java.lang.Float.TYPE => java.lang.Float.valueOf(value)
      case java.lang.Long.TYPE => java.lang.Long.valueOf(value)
      case java.lang.Byte.TYPE => java.lang.Byte.valueOf(value)
      case java.lang.Short.TYPE => java.lang.Short.valueOf(value)
      case java.lang.Character.TYPE if (value.length == 1) => value.charAt(0)
      case _ => ReflectionManager.getClassInfo(clazz).newInstance(value)
    }
    v.asInstanceOf[T]
  }


  def convertToInstance(value: AnyRef): T =
    (
      if (clazz == classOf[Object]) value
      else if (clazz == classOf[String]) String.valueOf(value)
      // todo: bad assumption:
      // If both the expected type and actual type are primitives, then assume they match up correctly.
      else if (isPrimitive && ClassInfoImpl.defaultValues.containsKey(value.getClass)) value
      else if (value == null && !isPrimitive) null
      else
        value match {
          case StringList(x) => nestedStringListToInstance(x)
          case StringMap(x) => nestedStringListToInstance(x.toList)
          case list:List[_] if (genericParams.length == 1) => list.map(v => genericParams(0).convertToInstance(v.asInstanceOf[AnyRef]))
          case _ if isInstance(value) => value
          case x: String => newInstanceFromString(value.asInstanceOf[String])
          case _ => newInstance(value)
        }
      ).asInstanceOf[T]

  def nestedStringListToInstance(list: List[(AnyRef, AnyRef)]): T = {
    if (clazz.isAssignableFrom(classOf[List[_]])) {
      if (genericParams.length == 1) list.map(genericParams(0).convertToInstance(_))
      else list
    } else clazz match {
      case ReflectionManager.mapClass if (genericParams.length == 2) => Map.empty ++ list.map(e => (genericParams(0).convertToInstance(e._1), genericParams(1).convertToInstance(e._2)))
      case ReflectionManager.mapClass => Map.empty ++ list
      case _ =>
        val instance = newInstance()
        list.foreach(e => getField(e._1.toString).map(_.convertAndSet(instance.asInstanceOf[AnyRef], e._2)))
        instance
    }
  }.asInstanceOf[T]

}


















