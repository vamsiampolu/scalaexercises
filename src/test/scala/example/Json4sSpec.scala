package example

import org.scalatest._
import java.time._
import java.time.format._

import org.json4s.{CustomSerializer, DefaultFormats}
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization.{read, write}
import org.json4s.JsonAST.{JString, JNull}

object LocalDateSerializer extends CustomSerializer[LocalDate](format => ({
  case JString(str) => LocalDate.parse(str)
  case JNull => null
}, {
    case value: LocalDate  => {
      val formatter = DateTimeFormatter.ofPattern("YYYY-MM-dd")
      JString(formatter.format(value))
    }
  }
))

case class Dates(createdAt: LocalDate, updatedAt: LocalDate, startDate: LocalDate, endDate: LocalDate )

case class Employee(
  firstName: String,
  lastName: String,
  designation: String,
  joiningDate: LocalDate,
  dateOfBirth: LocalDate,
  middleName: Option[String]
)

case class Manager(
  identity: Employee,
  division: String,
  charecterstics: List[String],
  reportsTo: Employee,
  manages: List[Employee]
)

case class Task(
  name: String,
  assignedTo: Employee,
  supervisor: Employee,
  dates: Dates,
  manager: Manager
)


class Json4sSpec extends FlatSpec with Matchers {
  behavior of "read"

  it should "read Java 8 Local Date Time from JSON String with CustomSerializer" in {

    implicit val formats =  org.json4s.DefaultFormats ++ List(LocalDateSerializer)


    val input =
      """
        |{
        |  "createdAt": "1999-12-10",
        |  "updatedAt": "1999-12-16",
        |  "startDate": "2000-01-02",
        |  "endDate": "2000-01-16"
        |}
      """.stripMargin

    val expected = Dates(
      LocalDate.of(1999,12,10),
      LocalDate.of(1999,12,16),
      LocalDate.of(2000,1,2),
      LocalDate.of(2000,1,16)
    )

    val result = read[Dates] { input }

    result should be(expected)
  }

  it should "parse a complex data structure without a formatter" in {
    val middleName:Option[String] = None
    implicit val formats =  org.json4s.DefaultFormats ++ List(LocalDateSerializer)
    val assignedTo = Employee(
      "Janet",
      "Smith",
      "Design Engineer",
      LocalDate.of(1994,6,6),
      LocalDate.of(1983,2,28),
      middleName
    )
    val supervisor = Employee(
      "Jamie",
      "Black",
      "Head of Design",
      LocalDate.of(1990,6,6),
      LocalDate.of(1974,2,28),
      middleName
     )

    val managerPerson = Employee(
      "Jack",
      "Wilshere",
      "VP Design",
      LocalDate.of(1980,6,26),
      LocalDate.of(1954,2,28),
      middleName
     )

    val ceo = Employee(
      "Jennifer",
      "Adams",
      "CEO",
      LocalDate.of(1985,6,26),
      LocalDate.of(1964,2,28),
      middleName
    )

    val manager = Manager(
      managerPerson, 
      "Design",
      List(
        "Old Fashioned",
        "Esoteric",
        "Pedantic"
      ),
      ceo,
      List[Employee](assignedTo, supervisor)
    )

    val dates = Dates(
      LocalDate.of(1999,12,10),
      LocalDate.of(1999,12,16),
      LocalDate.of(2000,1,2),
      LocalDate.of(2000,1,16)
    )

    val expected = Task(
      "Obsidian",
      assignedTo,
      supervisor,
      dates,
      manager
    )

    val input = """
      {
	"name": "Obsidian",
	"assignedTo": {
		"firstName": "Janet",
		"lastName": "Smith",
		"designation": "Design Engineer",
		"joiningDate": "1994-06-06",
		"dateOfBirth": "1983-02-28"
	},
	"supervisor": {
		"firstName": "Jamie",
		"lastName": "Black",
		"designation": "Head of Design",
		"joiningDate": "1990-06-06",
		"dateOfBirth": "1974-02-28"
	},
	"manager": {
		"identity": {
			"firstName": "Jack",
			"lastName": "Wilshere",
			"designation": "VP Design",
			"joiningDate": "1980-06-26",
			"dateOfBirth": "1954-02-28"
		},
		"division": "Design",
		"charecterstics": [
			"Old Fashioned",
			"Esoteric",
			"Pedantic"
		],
		"reportsTo": {
			"firstName": "Jennifer",
			"lastName": "Adams",
			"designation": "CEO",
			"joiningDate": "1985-06-26",
			"dateOfBirth": "1964-02-28"
		},
		"manages": [{
				"firstName": "Janet",
				"lastName": "Smith",
				"designation": "Design Engineer",
				"joiningDate": "1994-06-06",
				"dateOfBirth": "1983-02-28"
			},
			{
				"firstName": "Jamie",
				"lastName": "Black",
				"designation": "Head of Design",
				"joiningDate": "1990-06-06",
				"dateOfBirth": "1974-02-28"
			}
		]
	},
	"dates": {
		"createdAt": "1999-12-10",
		"updatedAt": "1999-12-16",
		"startDate": "2000-01-02",
		"endDate": "2000-01-16"
	}
}
    """

    val result = read[Task] { input }

    result shouldBe a[Task]

    result.assignedTo shouldBe an[Employee]
    result.supervisor shouldBe an[Employee]

    result.manager shouldBe a[Manager]
    result.manager.identity shouldBe an[Employee]
    result.manager.reportsTo shouldBe an[Employee]

    result.manager.manages shouldBe a[List[Employee]]
    result.manager.charecterstics shouldBe a[List[String]]

    result.assignedTo.joiningDate shouldBe a[LocalDate]
    result.assignedTo.dateOfBirth shouldBe a[LocalDate]

    result should be(expected)
    result.manager.manages should be(List(assignedTo, supervisor))
  }

  behavior of "write"

  it should "accept a Scala object and write to JSON using custom formatter" in {
    implicit val formats =  org.json4s.DefaultFormats ++ List(LocalDateSerializer)

    val input = Dates(
      LocalDate.of(1999,12,10),
      LocalDate.of(1999,12,16),
      LocalDate.of(2000,1,2),
      LocalDate.of(2000,1,16)
    )

    val result = write[Dates](input)
    val parsedResult = parse(result)

    val createdAt = (parsedResult \ "createdAt")
    createdAt.values should be("1999-12-10")

    val updatedAt = (parsedResult \ "updatedAt")
    updatedAt.values should be("1999-12-16")

    val startDate = (parsedResult \ "startDate")
    startDate.values should be("2000-01-02")

    val endDate = (parsedResult \ "endDate")
    endDate.values should be("2000-01-16")
  }

}
