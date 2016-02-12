#written with Ryan Jelks on 2/08/16

require 'rspec'

def calc_price(books)
  price = 0
  until books.empty? do
    unique_book_set = books.uniq
    discount_multiplier = calc_discount_multiplier(unique_book_set)
    price += unique_book_set.length * 8 * discount_multiplier
    unique_book_set.each {|book| books.delete_at(books.index(book))}
  end
  price
end

def calc_discount_multiplier(num_books)
  case num_books.length
    when 2 then 0.95
    when 3 then 0.90
    when 4 then 0.80
    when 5 then 0.75
    else 1
  end
end

describe '#calc_price' do
  it 'should calculate prices' do
    expect(calc_price []).to eq 0
    expect(calc_price [1]).to eq 8
    expect(calc_price [1, 2]).to eq 15.2
    expect(calc_price [1, 2, 3]).to eq 21.6
    expect(calc_price [1, 2, 3, 4]).to eq 25.6
    expect(calc_price [1, 2, 3, 4, 5]).to eq 30
    expect(calc_price [1, 2, 3, 3]).to eq 29.6
    expect(calc_price [1, 1, 2, 2, 3, 3, 4, 5]).to eq 51.6
  end
end

#not working yet.
describe '#best_sets' do
  it 'should return the optimal sets of books' do
    expect(best_sets([1,1,2,2,3,3,4,5])).to eq([[1,2,3,4][1,2,3,5]])
  end
end

#not working yet.
def best_sets books

end