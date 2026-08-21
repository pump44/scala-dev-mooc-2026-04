package ru.otus.module4.homework.services

import io.getquill.context.ZioJdbc.QIO
import ru.otus.module4.homework.dao.entity.{Role, RoleCode, User}
import ru.otus.module4.homework.dao.repository.UserRepository
import ru.otus.module4.phoneBook.db
import zio.{ZIO, ZLayer}

import java.sql.SQLException

trait UserService {
  def listUsers(): QIO[List[User]]
  def listUsersDTO(): QIO[List[UserDTO]]
  def addUserWithRole(user: User, roleCode: RoleCode): QIO[UserDTO]
  def listUsersWithRole(roleCode: RoleCode): QIO[List[UserDTO]]
}
class Impl(userRepo: UserRepository) extends UserService {
  val dc = db.Ctx

  def listUsers(): QIO[List[User]] =
    userRepo.list()

  def listUsersDTO(): QIO[List[UserDTO]] = for {
    users <- userRepo.list()
    userRoles <- ZIO.foreach(users) { u =>
      userRepo
        .userRoles(u.typedId)
        .map(r => UserDTO(u, r.toSet))
    }
  } yield userRoles

  def addUserWithRole(user: User, roleCode: RoleCode): QIO[UserDTO] = for {
    tr <- dc
      .transaction {
        for {
          _user <- userRepo.createUser(user)
          _ <- userRepo.insertRoleToUser(
            roleCode = roleCode,
            userId = _user.typedId
          )
          userRoles <- userRepo.userRoles(_user.typedId)
        } yield UserDTO(_user, userRoles.toSet)
      }
      .mapError(e => SQLException(e))
  } yield tr

  def listUsersWithRole(roleCode: RoleCode): QIO[List[UserDTO]] = for {
    users <- userRepo.listUsersWithRole(roleCode)
    dto <- ZIO.foreach(users) { u =>
      userRepo.userRoles(u.typedId).map(r => UserDTO(u, r.toSet))
        .debug
    }
  } yield dto

}
object UserService {

  val layer: ZLayer[UserRepository, Nothing, UserService] =
    ZLayer.fromFunction(new Impl(_))
}

case class UserDTO(user: User, roles: Set[Role])
