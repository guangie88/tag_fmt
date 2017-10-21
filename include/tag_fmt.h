/**
 * Implementation of header-only tag_fmt in entirety.
 * @author Chen Weiguang
 * @version 0.1.0
 */

#pragma once

#ifndef FMT_HEADER_ONLY
#define FMT_HEADER_ONLY
#endif

#include "fmt/format.h"
#include "mpark/variant.hpp"
#include "rustfp/let.h"
#include "rustfp/option.h"
#include "rustfp/result.h"
#include "rustfp/unit.h"

#include <algorithm>
#include <exception>
#include <functional>
#include <iterator>
#include <limits>
#include <memory>
#include <string>
#include <unordered_map>
#include <utility>

namespace tag_fmt {

    // declaration section

    namespace details {
        /** 
         * Provides description of the type that the tag value holds.
         */
         enum class tag_value_type {
            bool_type,
            i8_type, i16_type, i32_type, i64_type,
            u8_type, u16_type, u32_type, u64_type,
            f32_type, f64_type,
            str_type,
        };

        /**
         * Contains the actual type and value for the tag captured
         * in the tag formatter in a sum type container (variant).
         */
        class tag_value {
        public:
            /**
             * Alias to the internal variant type.
             */
            using value_t = mpark::variant<
                bool,
                int8_t, int16_t, int32_t, int64_t,
                uint8_t, uint16_t, uint32_t, uint64_t,
                float, double, long double,
                std::string>;
            
            /**
             * Alias to the internal variant type wrapped in Option.
             */
            using opt_value_t = rustfp::Option<value_t>;

            /**
             * Creates a copy of the given value and moves into internal
             * storage.
             * @tparam T Valid value type.
             * @param value Created value for storage.
             */
            template <class T>
            explicit tag_value(T value);

            /**
             * Special constructor to take in char array/pointer and store as
             * std::string.
             * @param value Char array/pointer to be contained.
             */
            tag_value(const char value[]);
             
            /**
             * Obtains the tag value.
             * @return Contained tag value type.
             */
            auto get_type() const -> rustfp::Option<tag_value_type>;

            /**
             * Obtains a newly created value based on the provided template type
             * using the value stored in this instance.
             * @tparam Value type to obtain.
             * @return Contained value as type T.
             */
            template <class T>
            auto get_value() const -> rustfp::Option<const T &>;

        private:
            opt_value_t value;
        };
    }

    /**
     * Contains a list of tag_rpms between tag name to its string value.
     */
    class formatter {
    public:
        /**
         * Applies stored list of tag_rpms for replacement on the given string
         * content. Works on char array that is null terminated for efficiency
         * sake.
         * @param content str_type content which should contain the tags for
         * string replacements. Must be null terminated for defined behaviour.
         */
        auto apply(const char content[]) const ->
            rustfp::Result<std::string, std::string>;

        /**
         * Applies stored list of tag_rpms for replacement on the given string
         * content. Works on std::string type.
         * @param content str_type content which should contain the tags for
         * string replacements.
         */
        auto apply(const std::string &content) const ->
            rustfp::Result<std::string, std::string>;

        /**
         * Sets the mapping between tag name to the string value to
         * replace into. Name of tag should not include the curly brackets.
         * Works only on lvalues.
         * @tparam T Valid value type.
         * @param tag Name of tag to replace from.
         * @param value Valid value to replace tag into.
         */
        template <class T>
        auto set_mapping(const std::string &tag, T &&value) & -> formatter &;

        /**
         * Sets the mapping between tag name to the string value to
         * replace into. Name of tag should not include the curly brackets.
         * Works only on rvalue or rvalue ref to express builder pattern.
         * @tparam T Valid value type.
         * @param tag Name of tag to replace from.
         * @param value valid value to replace tag into.
         */
        template <class T>
        auto set_mapping(const std::string &tag, T &&value) && -> formatter;

    private:
        /**
         * Contains the tag_rpms between the tag name to the
         * intended string value replacements.
         */
        std::unordered_map<std::string, details::tag_value> tag_rpms;
    };

    /**
     * Creates the formatter to perform tag replacement.
     */
    auto make_formatter() -> formatter;

    // implementation section

    namespace details {        
        /**
         * Describes the current state of parsing for tag formatter.
         */
        enum class tag_state {
            normal_text,
            open_curly_bracket,
            close_curly_bracket,
            capturing_tag,
            capturing_formatting,
            skip_capturing,
            skip_capturing_close_curly_bracket,
            error,
            end,
        };

        template <class T>
        tag_value::tag_value(T value) :
            value(rustfp::Some(value_t(std::move(value)))) {
            
        }

        inline tag_value::tag_value(const char value[]) :
            value(rustfp::Some(value_t(std::string(value)))) {
            
        }

        inline auto tag_value::get_type() const
            -> rustfp::Option<tag_value_type> {

            // mpark
            using mpark::get_if;

            // rustfp
            using rustfp::None;
            using rustfp::Some;

            return value.match(
                [](const auto &val) -> rustfp::Option<tag_value_type> {
                    if (get_if<bool>(&val)) {
                        return Some(tag_value_type::bool_type);
                    } else if (get_if<int8_t>(&val)) {
                        return Some(tag_value_type::i8_type);
                    } else if (get_if<int16_t>(&val)) {
                        return Some(tag_value_type::i16_type);
                    } else if (get_if<int32_t>(&val)) {
                        return Some(tag_value_type::i32_type);
                    } else if (get_if<int64_t>(&val)) {
                        return Some(tag_value_type::i64_type);
                    } else if (get_if<uint8_t>(&val)) {
                        return Some(tag_value_type::u8_type);
                    } else if (get_if<uint16_t>(&val)) {
                        return Some(tag_value_type::u16_type);
                    } else if (get_if<uint32_t>(&val)) {
                        return Some(tag_value_type::u32_type);
                    } else if (get_if<uint64_t>(&val)) {
                        return Some(tag_value_type::u64_type);
                    } else if (get_if<float>(&val)) {
                        return Some(tag_value_type::f32_type);
                    } else if (get_if<double>(&val)) {
                        return Some(tag_value_type::f64_type);
                    } else if (get_if<std::string>(&val)) {
                        return Some(tag_value_type::str_type);
                    } else {
                        // undefined case
                        return None;
                    }
                },

                [] { return None; });
        }

        template <class T>
        auto tag_value::get_value() const -> rustfp::Option<const T &> {
            return value.match(
                [](const auto &val) -> rustfp::Option<const T &> {
                    auto val_ptr = mpark::get_if<T>(&val);

                    if (val_ptr) {
                        return rustfp::Some(std::cref(*val_ptr));
                    } else {
                        return rustfp::None;
                    }
                },

                [] { return rustfp::None; });
        }

        /**
         * Executes one step of the state machine and returns the state
         * after execution. The accumulated result of the parsed string
         * is stored into machine_res.
         */
        inline auto exec_tag_state_machine(
            const char fmt_char,
            const tag_state state,
            const std::unordered_map<std::string, tag_value> &tag_rpms,
            std::vector<char> &machine_buf,
            std::vector<char> &machine_res)
            -> rustfp::Result<tag_state, std::string> {

            // fmt
            using fmt::format;

            // rustfp
            using rustfp::Err;
            using rustfp::Ok;
            using rustfp::Result;
            using rustfp::Unit;
            using rustfp::unit_t;

            // std
            using std::cbegin;
            using std::cend;
            using std::cref;
            using std::find_first_of;
            using std::string;
            using std::vector;

            const auto find_tag_value = [&tag_rpms](const string &tag_name) ->
                Result<const tag_value &, string> {

                const auto tap_rpms_itr = tag_rpms.find(tag_name);

                if (tap_rpms_itr == tag_rpms.cend()) {
                    return Err(format(
                        "No matching tag value for tag name '{}'!", tag_name));
                }

                return Ok(cref(tap_rpms_itr->second));
            };

            const auto insert_fmt_value =
                [&find_tag_value](
                    const vector<char> &imbued_tag_and_fmt,
                    vector<char> &result) -> Result<unit_t, string> {

                static constexpr char TARGET_CHARS[] = ":}";

                const auto imbued_tag_split_itr = find_first_of(
                    imbued_tag_and_fmt.cbegin(), imbued_tag_and_fmt.cend(),
                    cbegin(TARGET_CHARS), cend(TARGET_CHARS));

                const string tag_name(
                    imbued_tag_and_fmt.cbegin() + 1,
                    imbued_tag_split_itr);

                auto tag_res = find_tag_value(tag_name);
                RUSTFP_LET_REF(tag, tag_res);

                const auto imbued_fmt = "{" + string(
                    imbued_tag_split_itr, imbued_tag_and_fmt.cend());

                const auto get_fmt_value = [](
                    const tag_value &tag,
                    const string &imbued_fmt) -> Result<string, string> {

                    auto tag_type_res = tag.get_type().ok_or_else([] {
                        return format(
                            "Tag value is in None state, which is unexpected!");
                    });
    
                    RUSTFP_LET_REF(tag_type, tag_type_res);

                    switch (tag_type) {
                    case tag_value_type::bool_type:
                        return Ok(format(
                            imbued_fmt,
                            tag.template get_value<bool>().get_unchecked()));
                        
                    case tag_value_type::i8_type:
                        return Ok(format(
                            imbued_fmt,
                            tag.template get_value<int8_t>().get_unchecked()));
                        
                    case tag_value_type::i16_type:
                        return Ok(format(
                            imbued_fmt,
                            tag.template get_value<int16_t>().get_unchecked()));
                        
                    case tag_value_type::i32_type:
                        return Ok(format(
                            imbued_fmt,
                            tag.template get_value<int32_t>().get_unchecked()));
                        
                    case tag_value_type::i64_type:
                        return Ok(format(
                            imbued_fmt,
                            tag.template get_value<int64_t>().get_unchecked()));
                        
                    case tag_value_type::u8_type:
                        return Ok(format(
                            imbued_fmt,
                            tag.template get_value<uint8_t>().get_unchecked()));
                        
                    case tag_value_type::u16_type:
                        return Ok(format(
                            imbued_fmt,
                            tag.template get_value<uint16_t>().get_unchecked()));
                        
                    case tag_value_type::u32_type:
                        return Ok(format(
                            imbued_fmt,
                            tag.template get_value<uint32_t>().get_unchecked()));
                        
                    case tag_value_type::u64_type:
                        return Ok(format(
                            imbued_fmt,
                            tag.template get_value<uint64_t>().get_unchecked()));
                        
                    case tag_value_type::f32_type:
                        return Ok(format(
                            imbued_fmt,
                            tag.template get_value<float>().get_unchecked()));
                        
                    case tag_value_type::f64_type:
                        return Ok(format(
                            imbued_fmt,
                            tag.template get_value<double>().get_unchecked()));
                        
                    case tag_value_type::str_type:
                        return Ok(format(
                            imbued_fmt,
                            tag.template get_value<std::string>().get_unchecked()));
                                            
                    default:
                        return Err(string(
                            "Attempting to use an already moved tag value!"));
                    }
                };

                auto fmt_value_res = get_fmt_value(tag, imbued_fmt);
                RUSTFP_LET_REF(fmt_value, fmt_value_res);

                result.insert(
                    result.cend(),
                    fmt_value.cbegin(), fmt_value.cend());

                return Ok(Unit);
            };

            const auto transfer_buf_to_res = [](
                vector<char> &buffer, vector<char> &result) {

                result.insert(result.cend(), buffer.cbegin(), buffer.cend());
                buffer.clear();
            };
            
            auto next_state = tag_state::error;

            switch (state)
            {
            case tag_state::normal_text:
            case tag_state::error:
                switch (fmt_char) {
                case '{':
                    next_state = tag_state::open_curly_bracket;
                    machine_buf.push_back(fmt_char);
                    break;

                case '\0':
                    next_state = tag_state::end;
                    break;

                default:
                    next_state = state;
                    machine_res.push_back(fmt_char);
                    break;
                }

                break;

            case tag_state::open_curly_bracket:
                switch (fmt_char) {
                case '{':
                    // two open curly bracket detected
                    // revert back to normal state
                    next_state = tag_state::normal_text;
                    transfer_buf_to_res(machine_buf, machine_res);
                    machine_res.push_back(fmt_char);
                    break;

                case '}':
                    next_state = tag_state::skip_capturing_close_curly_bracket;
                    transfer_buf_to_res(machine_buf, machine_res);
                    machine_res.push_back(fmt_char);
                    break;

                case ':':
                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                    next_state = tag_state::skip_capturing;
                    transfer_buf_to_res(machine_buf, machine_res);
                    machine_res.push_back(fmt_char);
                    break;

                case '\0':
                    next_state = tag_state::end;
                    transfer_buf_to_res(machine_buf, machine_res);
                    break;

                default:
                    next_state = tag_state::capturing_tag;
                    machine_buf.push_back(fmt_char);
                    break;
                }

                break;

            case tag_state::capturing_tag:
                switch (fmt_char) {
                case '}':
                    next_state = tag_state::close_curly_bracket;
                    machine_buf.push_back(fmt_char);
                    break;

                case ':':
                    next_state = tag_state::capturing_formatting;
                    machine_buf.push_back(fmt_char);
                    break;

                case '\0':
                    next_state = tag_state::end;
                    transfer_buf_to_res(machine_buf, machine_res);
                    break;

                default:
                    next_state = tag_state::capturing_tag;
                    machine_buf.push_back(fmt_char);
                    break;
                }

                break;

            case tag_state::capturing_formatting:
                switch (fmt_char) {
                case '}':
                    next_state = tag_state::close_curly_bracket;
                    machine_buf.push_back(fmt_char);
                    break;

                case '\0':
                    next_state = tag_state::end;
                    transfer_buf_to_res(machine_buf, machine_res);
                    break;

                default:
                    next_state = state;
                    machine_buf.push_back(fmt_char);
                    break;
                }

                break;

            case tag_state::close_curly_bracket:
                switch (fmt_char) {
                case '}':
                    next_state = tag_state::error;
                    transfer_buf_to_res(machine_buf, machine_res);
                    machine_res.push_back(fmt_char);
                    break;

                case '{': {
                    next_state = tag_state::open_curly_bracket;

                    // insert tag value
                    auto insert_res = insert_fmt_value(
                        machine_buf, machine_res);

                    RUSTFP_RET_IF_ERR(insert_res);

                    machine_buf.clear();
                    machine_buf.push_back(fmt_char);

                    break;
                }

                case '\0': {
                    next_state = tag_state::end;

                    // insert tag value
                    auto insert_res = insert_fmt_value(
                        machine_buf, machine_res);

                    RUSTFP_RET_IF_ERR(insert_res);
                    machine_buf.clear();

                    break;
                }

                default: {
                    next_state = tag_state::normal_text;

                    // insert tag value
                    auto insert_res = insert_fmt_value(
                        machine_buf, machine_res);

                    RUSTFP_RET_IF_ERR(insert_res);

                    machine_buf.clear();
                    machine_res.push_back(fmt_char);

                    break;
                }}

                break;

            case tag_state::skip_capturing:
                switch (fmt_char) {
                case '}':
                    next_state = tag_state::skip_capturing_close_curly_bracket;
                    machine_buf.push_back(fmt_char);
                    break;

                case '\0':
                    next_state = tag_state::end;
                    transfer_buf_to_res(machine_buf, machine_res);
                    break;

                default:
                    next_state = state;
                    machine_res.push_back(fmt_char);
                    break;
                }

                break;

            case tag_state::skip_capturing_close_curly_bracket:
                switch (fmt_char) {
                case '}':
                    next_state = tag_state::error;
                    transfer_buf_to_res(machine_buf, machine_res);
                    machine_res.push_back(fmt_char);
                    break;

                case '{':
                    next_state = tag_state::open_curly_bracket;
                    transfer_buf_to_res(machine_buf, machine_res);
                    machine_buf.push_back(fmt_char);
                    break;

                case '\0':
                    next_state = tag_state::end;
                    transfer_buf_to_res(machine_buf, machine_res);
                    break;

                default:
                    next_state = tag_state::normal_text;
                    transfer_buf_to_res(machine_buf, machine_res);
                    machine_res.push_back(fmt_char);
                    break;
                }

                break;

            case tag_state::end:
                next_state = tag_state::end;
                break;
            }

            return Ok(next_state);
        }
    }

    inline auto formatter::apply(const char content[]) const ->
        rustfp::Result<std::string, std::string> {

        // std
        using std::exception;
        using std::move;
        using std::string;
        using std::vector;

        // rustfp
        using rustfp::Err;
        using rustfp::Ok;
        using rustfp::Result;
        using rustfp::Unit;
        using rustfp::unit_t;

        try {
            auto state = details::tag_state::normal_text;
            vector<char> machine_buf;
            vector<char> machine_res;

            const auto manage_tag_state_machine =
                [this, &machine_buf, &machine_res, &state](const char fmt_char)
                    -> Result<unit_t, string> {

                    auto state_res = details::exec_tag_state_machine(
                        fmt_char, state, tag_rpms, machine_buf, machine_res);

                    RUSTFP_LET(state_buf, state_res);
                    state = state_buf;
                    return Ok(Unit);
                };
    
            for (size_t index = 0; content[index] != '\0'; ++index) {
                auto state_res = manage_tag_state_machine(content[index]);
                RUSTFP_RET_IF_ERR(state_res);
            }

            // needs the additional null character to end off the process
            auto state_res = manage_tag_state_machine('\0');
            RUSTFP_RET_IF_ERR(state_res);
            return Ok(string(machine_res.cbegin(), machine_res.cend()));

        } catch (const exception &e) {
            return Err(string(e.what()));
        }
    }

    inline auto formatter::apply(const std::string &content) const ->
        rustfp::Result<std::string, std::string> {

        return apply(content.c_str());
    }

    template <class T>
    auto formatter::set_mapping(const std::string &tag, T &&value) &
        -> formatter & {

        auto tag_rpm_itr = tag_rpms.find(tag);

        if (tag_rpm_itr == tag_rpms.cend()) {
            // empty, so emplace new value
            tag_rpms.emplace(tag, details::tag_value(std::forward<T>(value)));
        } else {
            // key exists, so replace the value
            tag_rpm_itr->second = details::tag_value(std::forward<T>(value));
        }

        return *this;
    }

    template <class T>
    auto formatter::set_mapping(const std::string &tag, T &&value) &&
        -> formatter {

        // calls the lvalue version
        return std::move(this->set_mapping(tag, std::forward<T>(value)));
    }

    inline auto make_formatter() -> formatter {
        return formatter{};
    }
}
