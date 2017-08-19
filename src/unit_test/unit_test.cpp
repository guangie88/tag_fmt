#include "tag_fmt.h"

#define CATCH_CONFIG_MAIN
#include "catch.hpp"

#include <string>
#include <utility>

// std
using std::move;
using std::string;

TEST_CASE("Simple tag formatter test case #0", "[simple_0]") {
    static constexpr auto STR = "Hello World";
    const auto formatter = ::tag_fmt::make_formatter();

    auto apply_res = formatter.apply(STR);
    REQUIRE(apply_res.is_ok());

    const auto replaced_content = move(apply_res).unwrap_unchecked();
    REQUIRE(replaced_content == STR);
}

TEST_CASE("Simple tag formatter test case #1", "[simple_1]") {
    static constexpr auto STR = "Hello {w}";

    const auto formatter = ::tag_fmt::make_formatter()
        .set_mapping("w", "World");
    
    auto apply_res = formatter.apply(STR);
    REQUIRE(apply_res.is_ok());

    const auto replaced_content = move(apply_res).unwrap_unchecked();
    REQUIRE(replaced_content == "Hello World");
}

TEST_CASE("Simple tag formatter test case #2", "[simple_2]") {
    static constexpr auto STR = "{h} {w}";

    const auto formatter = ::tag_fmt::make_formatter()
        .set_mapping("w", "Water?")
        .set_mapping("h", "Hello")
        .set_mapping("w", "World");
    
    auto apply_res = formatter.apply(STR);
    REQUIRE(apply_res.is_ok());

    const auto replaced_content = move(apply_res).unwrap_unchecked();
    REQUIRE(replaced_content == "Hello World");
}

TEST_CASE("Simple tag formatter test case #3", "[simple_3]") {
    // meant to fail
    static constexpr auto STR = "{h} {w}";

    const auto formatter = ::tag_fmt::make_formatter();
    auto apply_res = formatter.apply(STR);
    REQUIRE(apply_res.is_err());
}
