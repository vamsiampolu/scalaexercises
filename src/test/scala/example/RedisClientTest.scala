package example

import com.redis._
import com.redis.serialization._
import org.scalatest._

class RedisClientStringSpec extends FlatSpec with Matchers with BeforeAndAfterAll with BeforeAndAfterEach {

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
    redisClient.flushall
    super.afterEach 
  }

  override def afterAll: Unit = {
    redisClient.close
    super.afterAll
  }

  behavior of "RedisClient#ping"


  it should "respond with a PONG" in {
    redisClient.ping shouldEqual Some("PONG")
  }

  behavior of "RedisClient#keys"

  it should "retrieve all the keys" in {
    redisClient.set("foo", "bar") 
    redisClient.keys("*")  shouldEqual Some(List(Some("foo")))
  }

  behavior of "RedisClient#dbsize"

  it should "retrieve the dbsize" in {
    redisClient.dbsize shouldEqual Some(0)
    redisClient.set("foo", "bar") 
    redisClient.set("bar", "baz")
    redisClient.dbsize shouldEqual Some(2)
  }


  behavior of "RedisClient#del"

  it should "delete a key" in {
     val key = "foo"
     redisClient.set(key, "bar") 
     redisClient.del(key) shouldEqual Some(1L)
     redisClient.keys("*") shouldEqual Some(List())
  }


  behavior of "RedisClient#set and RedisClient#get"

  it should "set a string value to a key" in {
    val key = "foo";
    val value = "bar";

    redisClient.set(key, value) shouldBe true

    val actual = redisClient.get(key)
    actual shouldEqual Some(value)
  }


  it should "return None if an existing key has been deleted" in {
     val key = "foo"
     redisClient.set(key, "bar") 
     redisClient.del(key)
     redisClient.get(key) shouldEqual None
  }



  behavior of "RedisClient#exists"
  
  it should "return true if key exists" in {
    val key = "foo"
    redisClient.set(key, "bar") 
    redisClient.exists(key) shouldEqual true
  }

  it should "return false if key does not exist" in {
    val key = "foo"; 
    redisClient.set("bar", "baz")
    redisClient.exists(key) shouldEqual false
  }

  behavior of "RedisClient#rename"

  it should "rename old key to new key when new key does not exist" in {
    val oldKey = "may" 
    val newKey = "can"
    val value = "bee"

    redisClient.set(oldKey, value)
    redisClient.rename(oldKey, newKey)

    redisClient.exists(oldKey) shouldBe false
    redisClient.exists(newKey) shouldBe true
    redisClient.get(newKey) shouldEqual Some(value)
  }

  it should "override the value of new key if already exists?" in {
    val oldKey = "may" 
    val newKey = "can"
    val value = "bee"

    redisClient.set(oldKey, value)
    redisClient.set(newKey, "ada")
    redisClient.rename(oldKey, newKey)

    redisClient.exists(oldKey) shouldBe false
    redisClient.exists(newKey) shouldBe true
    redisClient.get(newKey) shouldEqual Some(value)
  }

  behavior of "RedisClient#renamenx"

  it should "rename oldKey to newKey where newKey does not exist" in {
    val oldKey = "may" 
    val newKey = "can"
    val value = "bee"

    redisClient.set(oldKey, value)

    redisClient.renamenx(oldKey, newKey) shouldBe true

    redisClient.exists(oldKey) shouldBe false
    redisClient.exists(newKey) shouldBe true
    redisClient.get(newKey) shouldEqual Some(value)
  }

  it should "throw an error if the newKey exists" in  {
    val oldKey = "may" 
    val newKey = "can"
    val value = "bee"

    redisClient.set(oldKey, value)
    redisClient.set(newKey, "ada")

    redisClient.renamenx(oldKey, newKey) shouldBe false

    redisClient.exists(oldKey) shouldBe true 
    redisClient.exists(newKey) shouldBe true
    redisClient.get(newKey) shouldEqual Some("ada")
  }

  behavior of "RedisClient#expire, RedisClient#pexpire, RedisClient#persist"

  it should "remove a key after an expiry of n seconds has been set" in {
    val key = "foo" 
    val timeInSeconds = 3
    redisClient.set(key, "bar");

    redisClient.expire(key, timeInSeconds)
    
    // Thread.sleep takes milliseconds
    Thread.sleep(timeInSeconds * 1000)
    redisClient.exists(key) shouldEqual false
  }

it should "remove a key after an pexpire of n milliseconds has been set" in {
    val key = "foo" 
    val timeInMilliseconds = 3 * 1000
    redisClient.set(key, "bar");

    redisClient.pexpire(key, timeInMilliseconds)
    
    Thread.sleep(timeInMilliseconds)
    redisClient.exists(key) shouldEqual false
  }

  it should "not remove the key if persist is used before the expiry of n seconds" in {
   val key = "foo" 
   val timeInSeconds = 3
   redisClient.set(key, "bar")

   redisClient.expire(key, timeInSeconds);
   redisClient.persist(key)
   Thread.sleep(timeInSeconds * 1000)
   redisClient.exists(key) shouldEqual true
  }
}
