-record(matrix, {
    pixels = #{} :: #{{non_neg_integer(), non_neg_integer()} => non_neg_integer()},
    width = 128 :: non_neg_integer(),
    height = 128 :: non_neg_integer()
}).