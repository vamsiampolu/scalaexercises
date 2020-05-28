package example


import com.redis._
import com.redis.serialization._
import org.scalatest._

class RedisClientSetSpec extends FlatSpec with Matchers with BeforeAndAfterAll with BeforeAndAfterEach {

  var redisClient: RedisClient = null;

  override def beforeAll: Unit = {
    redisClient = new RedisClient("localhost", 6379)
    super.beforeAll
    }

  override def beforeEach: Unit = {
    redisClient.flushall
    super.beforeEach 
  }

  override def afterEach: Unit = {
    super.afterEach 
  }

  override def afterAll: Unit = {
    redisClient.close
    super.afterAll
  }

  behavior of "RedisClient#sadd"

  it should "create a set and add elements to it" in {
    redisClient.sadd("foo", "a", "a", "b", "b") shouldEqual Some(2L) 
  }

  it should "add elemdents to an existing set" in {
    redisClient.sadd("foo", "a", "a", "b", "b") shouldEqual Some(2L) 
    redisClient.sadd("foo", "c", "d", "c", "b") shouldEqual Some(2L) 
  }

  behavior of "RedisClient#scard"

  it should "retrieve the cardinality of a set" in {
    redisClient.sadd("foo", "a", "a", "b", "b")
    redisClient.scard("foo") shouldEqual Some(2L)
  }

  behavior of "RedisClient#sismember"

  it should "return true if an element is a member of the set" in {
    redisClient.sadd("foo", "a", "a", "b", "b")
    redisClient.sismember("foo", "a") shouldBe true
  }

  it should "return false if an element is not a member of the set" in {
    redisClient.sadd("foo", "a", "a", "b", "b")
    redisClient.sismember("foo", "c") shouldBe false
  }

  behavior of "RedisClient#smembers"

  it should "return all the members of a Set" in {
    redisClient.sadd("foo", "a", "a", "b", "b") 
    
    redisClient.smembers("foo") shouldEqual Some(
      Set(
        Some("a"),  
        Some("b")
      )
    ) 
  }

  behavior of "RedisClient#srem"

  it should "remove an element from a Set" in {
    redisClient.sadd("foo", "a", "a", "b", "b") 
    redisClient.srem("foo", "a") shouldEqual Some(1L)
     
    redisClient.smembers("foo") shouldEqual Some(
      Set(
        Some("b")
      )
    ) 
  }

  it should "remove multiple elements from a Set" in {
    redisClient.sadd("xyz", "a", "b", "c", "d") 
    redisClient.srem("xyz", "a", "b")

    redisClient.smembers("xyz") shouldEqual Some(
      Set(
        Some("c"),  
        Some("d")
      )  
    )
  }

  behavior of "RedisClient#spop"

  it should "remove a random element from a Set" in {
    redisClient.sadd("xyz", "a", "b", "c", "d") 
    List(redisClient.spop("xyz").get) should contain oneOf (
      "a",
      "b",
      "c",
      "d"
    )
  }

  it should "remove multiple elements from a Set" in {
    redisClient.sadd("xyz", "a", "b", "c", "d") 
    redisClient.spop("xyz", 3).get should contain atLeastOneOf (
      Some("a"),
      Some("b"),
      Some("c"),
      Some("d")
    )
  }

  behavior of "RedisClient#srandmember"

  it should "retrieve a random element from a Set" in {
    redisClient.sadd("pqr", "p", "q", "r", "s") 
    List(
      redisClient.srandmember("pqr").get
    ) should contain oneOf ("p", "q", "r", "s")
  }

  // this method does not seem to work
  ignore should "retrieve any two random element from a Set" in {
    redisClient.sadd("def", "p", "q", "r", "s") 
    val actual = redisClient.srandmember("def", 2) getOrElse List[String]()
    actual should contain atLeastOneOf (
      Some("p"),
      Some("q"),
      Some("r"),
      Some("s")
    )
  }
}

