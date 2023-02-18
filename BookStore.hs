data BookInfo = Book Int String [String]
                deriving (Show)
data MagazineInfo = Magazine Int String [String]
                deriving (Show)

type CustomerId = Int
type Review = String
data BookReview = BookReview BookInfo CustomerId Review

myInfo = Book 1234 "Hiren" ["Doshi", "Enterprise"]
