package referenceapi.test.models {
  object Arbitrary {
    import referenceapi.models._
    implicit def arbBig: org.scalacheck.Arbitrary[Big] = org.scalacheck.Arbitrary {
      for {
        f1 <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
        f2 <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
        f3 <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
        f4 <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
        f5 <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
        f6 <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
        f7 <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
        f8 <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
        f9 <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
        f10 <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
        f11 <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
        f12 <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
        f13 <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
        f14 <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
        f15 <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
        f16 <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
        f17 <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
        f18 <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
        f19 <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
        f20 <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
        f21 <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
      } yield {
        new Big(
          f1 = f1,
          f2 = f2,
          f3 = f3,
          f4 = f4,
          f5 = f5,
          f6 = f6,
          f7 = f7,
          f8 = f8,
          f9 = f9,
          f10 = f10,
          f11 = f11,
          f12 = f12,
          f13 = f13,
          f14 = f14,
          f15 = f15,
          f16 = f16,
          f17 = f17,
          f18 = f18,
          f19 = f19,
          f20 = f20,
          f21 = f21
        )
      }
    }
    
    implicit def arbBig_Patch = org.scalacheck.Arbitrary {
      for {
        f1 <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
        f2 <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
        f3 <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
        f4 <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
        f5 <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
        f6 <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
        f7 <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
        f8 <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
        f9 <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
        f10 <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
        f11 <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
        f12 <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
        f13 <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
        f14 <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
        f15 <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
        f16 <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
        f17 <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
        f18 <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
        f19 <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
        f20 <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
        f21 <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
      } yield {
        new Big.Patch(
        f1 = f1,
        f2 = f2,
        f3 = f3,
        f4 = f4,
        f5 = f5,
        f6 = f6,
        f7 = f7,
        f8 = f8,
        f9 = f9,
        f10 = f10,
        f11 = f11,
        f12 = f12,
        f13 = f13,
        f14 = f14,
        f15 = f15,
        f16 = f16,
        f17 = f17,
        f18 = f18,
        f19 = f19,
        f20 = f20,
        f21 = f21
        )
      }
    }
    
    implicit def arbSmall: org.scalacheck.Arbitrary[Small] = org.scalacheck.Arbitrary {
      for {
        f1 <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
      } yield {
        new Small(
          f1 = f1
        )
      }
    }
    
    implicit def arbSmall_Patch = org.scalacheck.Arbitrary {
      for {
        f1 <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
      } yield {
        new Small.Patch(
        f1 = f1
        )
      }
    }
    
    implicit def arbError: org.scalacheck.Arbitrary[Error] = org.scalacheck.Arbitrary {
      for {
        code <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
        message <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
      } yield {
        new Error(
          code = code,
          message = message
        )
      }
    }
    
    implicit def arbError_Patch = org.scalacheck.Arbitrary {
      for {
        code <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
        message <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
      } yield {
        new Error.Patch(
        code = code,
        message = message
        )
      }
    }
    
    implicit def arbOrganization: org.scalacheck.Arbitrary[Organization] = org.scalacheck.Arbitrary {
      for {
        guid <- org.scalacheck.Arbitrary(org.scalacheck.Gen.resultOf { _: Unit => java.util.UUID.randomUUID }).arbitrary
        name <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
      } yield {
        new Organization(
          guid = guid,
          name = name
        )
      }
    }
    
    implicit def arbOrganization_Patch = org.scalacheck.Arbitrary {
      for {
        guid <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.resultOf { _: Unit => java.util.UUID.randomUUID })).arbitrary
        name <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
      } yield {
        new Organization.Patch(
        guid = guid,
        name = name
        )
      }
    }
    
    implicit def arbUser: org.scalacheck.Arbitrary[User] = org.scalacheck.Arbitrary {
      for {
        guid <- org.scalacheck.Arbitrary(org.scalacheck.Gen.resultOf { _: Unit => java.util.UUID.randomUUID }).arbitrary
        email <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
        active <- org.scalacheck.Arbitrary.arbBool.arbitrary
      } yield {
        new User(
          guid = guid,
          email = email,
          active = active
        )
      }
    }
    
    implicit def arbUser_Patch = org.scalacheck.Arbitrary {
      for {
        guid <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.resultOf { _: Unit => java.util.UUID.randomUUID })).arbitrary
        email <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
        active <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary.arbBool).arbitrary
      } yield {
        new User.Patch(
        guid = guid,
        email = email,
        active = active
        )
      }
    }
    
    implicit def arbUserList: org.scalacheck.Arbitrary[UserList] = org.scalacheck.Arbitrary {
      for {
        users <- org.scalacheck.Arbitrary(org.scalacheck.Gen.listOf(org.scalacheck.Arbitrary {
          for {
            guid <- org.scalacheck.Arbitrary(org.scalacheck.Gen.resultOf { _: Unit => java.util.UUID.randomUUID }).arbitrary
            email <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
            active <- org.scalacheck.Arbitrary.arbBool.arbitrary
          } yield {
            new User(
              guid = guid,
              email = email,
              active = active
            )
          }
        }.arbitrary)).arbitrary
      } yield {
        new UserList(
          users = users
        )
      }
    }
    
    implicit def arbUserList_Patch = org.scalacheck.Arbitrary {
      for {
        users <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.listOf(org.scalacheck.Arbitrary {
          for {
            guid <- org.scalacheck.Arbitrary(org.scalacheck.Gen.resultOf { _: Unit => java.util.UUID.randomUUID }).arbitrary
            email <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
            active <- org.scalacheck.Arbitrary.arbBool.arbitrary
          } yield {
            new User(
              guid = guid,
              email = email,
              active = active
            )
          }
        }.arbitrary))).arbitrary
      } yield {
        new UserList.Patch(
        users = users
        )
      }
    }
    
    implicit def arbUserForm: org.scalacheck.Arbitrary[UserForm] = org.scalacheck.Arbitrary {
      for {
        email <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
      } yield {
        new UserForm(
          email = email
        )
      }
    }
    
    implicit def arbUserForm_Patch = org.scalacheck.Arbitrary {
      for {
        email <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
      } yield {
        new UserForm.Patch(
        email = email
        )
      }
    }
    
    implicit def arbMember: org.scalacheck.Arbitrary[Member] = org.scalacheck.Arbitrary {
      for {
        guid <- org.scalacheck.Arbitrary(org.scalacheck.Gen.resultOf { _: Unit => java.util.UUID.randomUUID }).arbitrary
        organization <- org.scalacheck.Arbitrary {
          for {
            guid <- org.scalacheck.Arbitrary(org.scalacheck.Gen.resultOf { _: Unit => java.util.UUID.randomUUID }).arbitrary
            name <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
          } yield {
            new Organization(
              guid = guid,
              name = name
            )
          }
        }.arbitrary
        user <- org.scalacheck.Arbitrary {
          for {
            guid <- org.scalacheck.Arbitrary(org.scalacheck.Gen.resultOf { _: Unit => java.util.UUID.randomUUID }).arbitrary
            email <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
            active <- org.scalacheck.Arbitrary.arbBool.arbitrary
          } yield {
            new User(
              guid = guid,
              email = email,
              active = active
            )
          }
        }.arbitrary
        role <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
      } yield {
        new Member(
          guid = guid,
          organization = organization,
          user = user,
          role = role
        )
      }
    }
    
    implicit def arbMember_Patch = org.scalacheck.Arbitrary {
      for {
        guid <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.resultOf { _: Unit => java.util.UUID.randomUUID })).arbitrary
        organization <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary {
          for {
            guid <- org.scalacheck.Arbitrary(org.scalacheck.Gen.resultOf { _: Unit => java.util.UUID.randomUUID }).arbitrary
            name <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
          } yield {
            new Organization(
              guid = guid,
              name = name
            )
          }
        }).arbitrary
        user <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary {
          for {
            guid <- org.scalacheck.Arbitrary(org.scalacheck.Gen.resultOf { _: Unit => java.util.UUID.randomUUID }).arbitrary
            email <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
            active <- org.scalacheck.Arbitrary.arbBool.arbitrary
          } yield {
            new User(
              guid = guid,
              email = email,
              active = active
            )
          }
        }).arbitrary
        role <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
      } yield {
        new Member.Patch(
        guid = guid,
        organization = organization,
        user = user,
        role = role
        )
      }
    }
    
    implicit def arbMemberForm: org.scalacheck.Arbitrary[MemberForm] = org.scalacheck.Arbitrary {
      for {
        organization <- org.scalacheck.Arbitrary(org.scalacheck.Gen.resultOf { _: Unit => java.util.UUID.randomUUID }).arbitrary
        user <- org.scalacheck.Arbitrary(org.scalacheck.Gen.resultOf { _: Unit => java.util.UUID.randomUUID }).arbitrary
        role <- org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr).arbitrary
      } yield {
        new MemberForm(
          organization = organization,
          user = user,
          role = role
        )
      }
    }
    
    implicit def arbMemberForm_Patch = org.scalacheck.Arbitrary {
      for {
        organization <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.resultOf { _: Unit => java.util.UUID.randomUUID })).arbitrary
        user <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.resultOf { _: Unit => java.util.UUID.randomUUID })).arbitrary
        role <- org.scalacheck.Arbitrary.arbOption(org.scalacheck.Arbitrary(org.scalacheck.Gen.alphaStr)).arbitrary
      } yield {
        new MemberForm.Patch(
        organization = organization,
        user = user,
        role = role
        )
      }
    }
  }
}
