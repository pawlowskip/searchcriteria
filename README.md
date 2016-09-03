# searchcriteria
This project is intended to provide a way to search in dbs or collections in easy and clean way. 
It is also build with idea of serialization such search criteria (to query string at first). 
Building search criteria is consise and clean using provided DSL.

As an example:
```scala
// model
case class Movie(id: Int,
                 title: String,
                 year: Int,
                 poster: Poster,
                 averageRating: Double,
                 viewers: Int,
                 description: String)

object Movie {
  // field accessors for searching
  case class MovieId(criteria: SCriteria[Int]) extends Field[Int, Movie](_.id)

  case class MovieTitle(criteria: SCriteria[String]) extends Field[String, Movie](_.title)

  case class MovieYear(criteria: SCriteria[Int]) extends Field[Int, Movie](_.year)

  case class MoviePoster(criteria: SCriteria[Poster]) extends Field[Poster, Movie](_.poster)

  case class MovieAverageRating(criteria: SCriteria[Double]) extends Field[Double, Movie](_.averageRating)

  case class MovieViewers(criteria: SCriteria[Int]) extends Field[Int, Movie](_.viewers)

  case class MovieDescription(criteria: SCriteria[String]) extends Field[String, Movie](_.description)

}

case class Poster(url: String)

object Poster{
  case class PosterUrl(criteria: SCriteria[String]) extends Field[String, Poster](_.url)
}

// deserializer with validation
val deserializer =
    create[Movie](
      And(
        MovieTitle(
          Equal[String](*) where (_.charAt(0).isUpper, "Title should starts with Upper letter.")
        ),
        MovieId(
          GreaterOrEqual[Int](*) where (_ >= 0, "Id should be greater or equal 0.")
        ),
        MovieYear(
          Between[Int](*, *) where (
            and(
              tuple2(_ < _),
              tuple2(_ > 1970, _ < 2009)
            ),
            failMessage = "'from' should be less than 'to', and ..."
            )
        ),
        MovieAverageRating(
          GreaterThan[Double](*) where betweenInl(0.0, 10.0)
        ),
        MovieDescription(
          OneOf(NotEmptyString, IsEmpty[String])
        ),
        MovieViewers(
          GreaterThan[Int](*) where(_ > 0)
        ),
        MoviePoster(
          PosterUrl(
            Equal[String](*) where (_.startsWith("url"))
          )
        )
      )
    ).withName("Movie")
     .where(
       option[SearchProps]{case SearchProps(limit, page) => page >= 0 && limit < 1000}(allowNone = true)
     )
     .getDeserializer

// example search criteria that can be serialized/deserialized and used for searching in collection or db
val criteria1 =
    create[Movie](
      And(
        MovieTitle(
          Equal("Movie title")
        ),
        MovieId(
          GreaterOrEqual(0)
        ),
        MovieYear(
          Between[Int](2000, 2005)
        ),
        MovieAverageRating(
          GreaterThan[Double](5.0)
        ),
        MovieDescription(
          NotEmptyString
        ),
        MovieViewers(
          GreaterThan[Int](1000)
        ),
        MoviePoster(
          PosterUrl(
            Equal[String]("url1")
          )
        )
      )
    ).withName("Movie").limit(100).page(0)
```