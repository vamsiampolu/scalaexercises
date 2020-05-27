package example

import com.redis._
import com.redis.serialization._
import org.scalatest._

class RedisClientListSpec extends FlatSpec with Matchers with BeforeAndAfterAll with BeforeAndAfterEach {

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

  behavior of "RedisClient#lpush and RedisClient#llen"

  it should "create a list with one element if key does not exist" in {
    redisClient.lpush("foo", "foo") shouldEqual Some(1L)
  }

  it should "add a multiple elements if key does not exist" in {
    redisClient.lpush("foo", "foo", "bar") shouldEqual Some(2L)
  }

  behavior of "RedisClient#llen"

  it should "be `Some(0)` if a key does not exist" in {
    redisClient.llen("car") shouldEqual Some(0) 
  }

  it should "retrieve the length of list for a key" in {
    redisClient.lpush("foo", "bar", "baz") 
    redisClient.llen("foo") shouldEqual Some(2)
  }

  behavior of "RedisClient#lrange"

  it should "get the whole list when a start index of 0 and end index of -1 is used" in {
    redisClient.lpush("foo", "bar", "baz") 
    // NOTE: The last variadic arg becomes the head of the List and the elements are inserted in the reverse order that they are specified in.
    redisClient.lrange("foo", 0, -1) shouldEqual Some(List(Some("baz"), Some("bar")))
  }

  it should "get the selected range when a start index and endIndex is specified" in {
    redisClient.lpush("foo", "d", "c", "b", "a") 
    redisClient.lrange("foo", 1, 3) shouldEqual Some(List(Some("b"), Some("c"), Some("d")))
  }

  behavior of "RedisClient#rpush"

  it should "create a List with an element if a key does not exist" in {
    redisClient.rpush("foo", "bar")  shouldEqual Some(1L)
    redisClient.llen("foo") shouldEqual Some(1)
    redisClient.lrange("foo", 0, -1) shouldEqual Some(List(Some("bar")))
  }

  it should "create a List with multiple elements if a key does not exist" in {
    redisClient.rpush("foo", "bar", "baz")  shouldEqual Some(2L)
    redisClient.llen("foo") shouldEqual Some(2)
    redisClient.lrange("foo", 0, -1) shouldEqual Some(List(Some("bar"), Some("baz")))
  }

  behavior of "RedisClient#lpop"

  it should "get the head of the List" in {
    redisClient.lpush("key", "bar", "baz")
    redisClient.lpop("key") shouldEqual Some("baz")
  }

  behavior of "RedisClient#rpop"

  it should "get the tail of the List" in {
    redisClient.lpush("fig", "leaf", "bear") 
    redisClient.rpop("fig") shouldEqual Some("leaf")
  }

  behavior of "RedisClient#ltrim"

  it should "remove all but first n elements of a List" in {
    redisClient.rpush("goo", 1,2,3,4,5,6,7,8); 
    redisClient.ltrim("goo", 0, 3) 
    redisClient.lrange("goo", 0, -1) shouldEqual Some(List(Some("1"), Some("2"), Some("3"), Some("4")))
  }
} 
