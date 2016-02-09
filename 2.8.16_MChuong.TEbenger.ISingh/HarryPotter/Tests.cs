using System.Collections.Generic;
using NUnit.Framework;

namespace HarryPotter
{
    [TestFixture]
    class Tests
    {
        [Test]
        public void BuyOneBook()
        {
            List<Book> shoppingCart = new List<Book>();
            var book = new Book(1);

            shoppingCart.Add(book);

            ShoppingCartCalculator calculator = new ShoppingCartCalculator();

            var actual = calculator.Calculate(shoppingCart);

            Assert.AreEqual(8M, actual);
        }

        [Test]
        public void BuyTwoBookOfDifferentType()
        {
            List<Book> shoppingCart = new List<Book>();
            var book = new Book(1);
            var book2 = new Book(2);

            shoppingCart.Add(book);
            shoppingCart.Add(book2);

            ShoppingCartCalculator calculator = new ShoppingCartCalculator();

            var actual = calculator.Calculate(shoppingCart);

            Assert.AreEqual(15.2M, actual);
        }


        [Test]
        public void BuyTwoBookOfSameType()
        {
            List<Book> shoppingCart = new List<Book>();
            var book = new Book(1);
            var book2 = new Book(1);

            shoppingCart.Add(book);
            shoppingCart.Add(book2);

            ShoppingCartCalculator calculator = new ShoppingCartCalculator();

            var actual = calculator.Calculate(shoppingCart);

            Assert.AreEqual(16M, actual);
        }

        [Test]
        public void BuyThreeBooksOfDifferentType()
        {
            List<Book> shoppingCart = new List<Book>();
            var book = new Book(1);
            var book2 = new Book(2);
            var book3 = new Book(3);

            shoppingCart.Add(book);
            shoppingCart.Add(book2);
            shoppingCart.Add(book3);

            ShoppingCartCalculator calculator = new ShoppingCartCalculator();

            var actual = calculator.Calculate(shoppingCart);

            Assert.AreEqual(21.6M, actual);
        }

        [Test]
        public void BuyEightBooksVariousType()
        {
            List<Book> shoppingCart = new List<Book>()
            {
                new Book(1),
                new Book(1),
                new Book(2),
                new Book(2),
                new Book(3),
                new Book(3),
                new Book(4),
                new Book(5),
            };

            ShoppingCartCalculator calculator = new ShoppingCartCalculator();

            var actual = calculator.Calculate(shoppingCart);

            Assert.AreEqual(51.20M, actual);
        }

        [Test]
        public void Buy148()
        {
            List<Book> shoppingCart = new List<Book>();

            for (int i = 0; i < 100; i++)
            {
                shoppingCart.Add(new Book(1));
            }
            for (int i = 0; i < 48; i++)
            {
                shoppingCart.Add(new Book(2));
            }

            ShoppingCartCalculator calculator = new ShoppingCartCalculator();

            var actual = calculator.Calculate(shoppingCart);

            Assert.AreEqual(1145.6M, actual);
        }

        [Test]
        public void BuyTwoSetsOfFiveAndTwoSetsOfThree()
        {
            var shoppingCart = new List<Book>();
            var calculator = new ShoppingCartCalculator();

            for (int i = 0; i < 4; i++)
            {
                shoppingCart.Add(new Book(1));
            }

            for (int i = 0; i < 4; i++)
            {
                shoppingCart.Add(new Book(2));
            }
            for (int i = 0; i < 4; i++)
            {
                shoppingCart.Add(new Book(3));
            }
            for (int i = 0; i < 2; i++)
            {
                shoppingCart.Add(new Book(4));
            }
            for (int i = 0; i < 2; i++)
            {
                shoppingCart.Add(new Book(5));
            }

            var actual = calculator.Calculate(shoppingCart);

            Assert.AreEqual(102.4M, actual);
        }
    }
}
