package example

import com.redis._
import com.redis.serialization._
import org.scalatest._
import scala.collection.Map

class RedisClientHashSpec extends FlatSpec with Matchers with BeforeAndAfterAll with BeforeAndAfterEach {

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

  behavior of "RedisClient#hset and RedisClient#hget"

  it should "add a key with a key-value pair" in {
    redisClient.hset("foo", "a", 1) shouldBe true
    redisClient.hget("foo", "a") shouldEqual Some("1")
  }

  it should "add a key-value pair to an existing hash" in {
    redisClient.hset("foo", "a", "1") shouldBe true
    redisClient.hset("foo", "b", "2") shouldBe true

    redisClient.hget("foo", "a") shouldEqual Some("1")
    redisClient.hget("foo", "b") shouldEqual Some("2")
  }

  behavior of "RedisClient#hmset and RedisClient#hmget"

  it should "add a group of key-value pairs to a key" in {
    val expected = Map(
      "red" -> "#FF0000",
      "azure" -> "#F0FFFF"  
    )

    redisClient.hmset("color-codes", expected) shouldEqual true
    redisClient.hmget("color-codes", "red", "azure") shouldEqual Some(expected)
  }

  behavior of "RedisClient#hsetnx"

  it should "add a key-value pair to a Redis key" in {
    redisClient.hmset("foo", Map(
      "a" -> "A",
      "b" -> "B"
    )) 

    redisClient.hsetnx("foo", "c", "C") shouldEqual true
    redisClient.hsetnx("foo", "a", "Z") shouldEqual false

    redisClient.hmget("foo", "a", "b", "c") shouldEqual Some(Map(
      "a" -> "A",   
      "b" -> "B",
      "c" -> "C"
    ))
  }

  behavior of "RedisClient#hexists"

  it should "return true if a field exists on a hash" in {
     redisClient.hmset("color-codes", Map(
      "red" -> "#FF0000",
      "azure" -> "#F0FFFF"  
    ))

     redisClient.hexists("color-codes", "azure") shouldBe true
  }

  it should "return false if a field does not exist on a hash" in {
     redisClient.hmset("color-codes", Map(
      "red" -> "#FF0000",
      "azure" -> "#F0FFFF"  
    ))

     redisClient.hexists("color-codes", "magenta") shouldBe false
  }

  behavior of "RedisClient#hincrby"

  it should "increment a field on a hash" in {
     redisClient.hmset("numbers", Map(
       "one" -> 1,
       "two" -> 1
     )) 
  
     redisClient.hincrby("numbers", "two", 1)
     redisClient.hget("numbers", "two") shouldEqual Some("2")

     redisClient.hincrby("numbers", "huh", 1)
     redisClient.hget("numbers", "huh") shouldEqual Some("1")
  }


  behavior of "RedisClient#hdel"

  it should "delete a single field from a hash" in {
     redisClient.hmset("color-codes", Map(
      "red" -> "#FF0000",
      "azure" -> "#F0FFFF"  
    ))

     redisClient.hdel("color-codes", "red") shouldEqual Some(1L)
     redisClient.hexists("color-codes", "red") shouldBe false

     // if a field does not exist on a key, it is not deleted
     redisClient.hdel("color-codes", "magenta") shouldEqual Some(0)
  }

  behavior of "RedisClient#hlen"

  it should "return the number of fields in the hash" in {
     redisClient.hmset("color-codes", Map(
      "red" -> "#FF0000",
      "azure" -> "#F0FFFF"  
    ))

     redisClient.hlen("color-codes") shouldEqual Some(2L)
  }

  behavior of "RedisClient#hkeys"
  
  it should "get the field names of the hash" in {
     redisClient.hmset("color-codes", Map(
      "red" -> "#FF0000",
      "azure" -> "#F0FFFF"  
    ))

     redisClient.hkeys("color-codes") shouldEqual Some(List("red", "azure"))
  }

  behavior of "RedisClient#hvals"

  it should "get the values for all fields in a hash" in {
    redisClient.hmset("shells", Map(
      "sh" -> "Shell",   
      "bsh" -> "Bourne Shell",
      "bash" -> "Bourne Again Shell",
      "zsh" -> "Z Shell",
      "fish" -> "Friendly Interactive Shell"
    )) 

    redisClient.hvals("shells").get should contain allOf (
      "Shell",
      "Bourne Shell",
      "Bourne Again Shell",
      "Z Shell",
      "Friendly Interactive Shell"
    )
  }
}
