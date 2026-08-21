package ru.otus.module4.homework.dao.repository

import zio.{ULayer, ZIO, ZLayer}
import io.getquill.context.ZioJdbc._
import ru.otus.module4.homework.dao.entity._
import ru.otus.module4.phoneBook.db
import io.getquill._
import io.getquill.autoQuote

import java.sql.SQLException
import javax.sql.DataSource

trait UserRepository{
    def findUser(userId: UserId): QIO[Option[User]]
    def createUser(user: User): QIO[User]
    def createUsers(users: List[User]): QIO[List[User]]
    def updateUser(user: User): QIO[Unit]
    def deleteUser(user: User): QIO[Unit]
    def findByLastName(lastName: String): QIO[List[User]]
    def list(): QIO[List[User]]
    def userRoles(userId: UserId): QIO[List[Role]]
    def insertRoleToUser(roleCode: RoleCode, userId: UserId): QIO[Unit]
    def listUsersWithRole(roleCode: RoleCode): QIO[List[User]]
    def findRoleByCode(roleCode: RoleCode): QIO[Option[Role]]
}


class UserRepositoryImpl extends UserRepository {
    val dc = db.Ctx
    import dc._

  inline def userSchema = quote {
    querySchema[User]("users")
  }

  inline def roleSchema = quote {
    querySchema[Role]("roles")
  }

  inline def userRoleSchema = quote {
    querySchema[UserToRole]("user_roles")
  }

    override def findUser(userId: UserId): QIO[Option[User]] =
      dc.run(userSchema.filter(_.id == lift(userId.id)).take(1)).map(_.headOption)

    override def createUser(user: User): QIO[User] =
      dc.run(userSchema.insertValue(lift(user))).as(user)

    override def createUsers(users: List[User]): QIO[List[User]] = {
      val q = quote {
        liftQuery(users).foreach {u =>
          userSchema.insertValue(u)
        }
      }
      dc.run(q).as(users)
    }

  override def updateUser(user: User): QIO[Unit] =
    dc.run(userSchema.filter(_.id == lift(user.id)).updateValue(lift(user))).unit

    override def deleteUser(user: User): QIO[Unit] =
      dc.run(userSchema.filter(_.id == lift(user.id)).delete).debug.unit

  override def findByLastName(lastName: String): QIO[List[User]] =
      dc.run(userSchema.filter(_.lastName == lift(lastName)))

    override def list(): QIO[List[User]] =
      dc.run(userSchema)

    override def userRoles(userId: UserId): QIO[List[Role]] = {
      val q = quote {
        for {
          ur   <- userRoleSchema
          role <- roleSchema
          if ur.userId == lift(userId.id)
          if ur.roleId == role.code
        } yield role
      }

      dc.run(q).debug
    }

    override def insertRoleToUser(roleCode: RoleCode, userId: UserId): QIO[Unit] =
      dc.run(userRoleSchema.insertValue(lift(UserToRole(roleCode.code, userId.id)))).unit

  override def listUsersWithRole(roleCode: RoleCode): QIO[List[User]] = {
    val q = quote {
      userRoleSchema
        .join(userSchema)
        .on(_.userId == _.id)
        .filter { case (ur, _) => ur.roleId == lift(roleCode.code) }
        .map(_._2)
    }

    dc.run(q)
  }

    override def findRoleByCode(roleCode: RoleCode): QIO[Option[Role]] =
      dc.run(roleSchema.filter(_.code == lift(roleCode.code)).take(1)).map(_.headOption)
}

object UserRepository{

    val layer: ULayer[UserRepository] = ZLayer.succeed(new UserRepositoryImpl)
}