/**
 * Implementation of header-only tag_fmt in entirety.
 * @author Chen Weiguang
 * @version 0.1.0
 */

#pragma once

#define FMT_HEADER_ONLY

#include "fmt/format.h"
#include "rustfp/let.h"
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
         * Deleter type alias for tag value.
         */
        using tag_value_deleter_t = void (*)(void * const);

        /**
         * Copier type alias for tag value.
         */
        using tag_value_copier_t = std::unique_ptr<void, tag_value_deleter_t>
            (*)(const std::unique_ptr<void, tag_value_deleter_t> &);

        /** 
         * Provides description of the type that the tag value holds.
         */
         enum class tag_value_type {
            invalid_type,
            bool_type,
            char_type, i16_type, i32_type, i64_type,
            u8_type, u16_type, u32_type, u64_type,
            f32_type, f64_type, long_double_type,
            str_type,
            ptr_type,
        };

        /**
         * Contains the actual type and value for the tag captured
         * in the tag formatter in a type erased form.
         */
        class tag_value {
        public:
            /**
             * Forwards the passed-in value into the constructed instance.
             * @tparam T Valid value type.
             * @param value Forwarded value to be contained.
             */
            template <class T>
            explicit tag_value(T &&value);

            /**
             * Special constructor to take in char array/pointer and store as std::string.
             * @param value Char array/pointer to be contained.
             */
            tag_value(const char value[]);

            /**
             * Copies the copier, value and tag value.
             * The value is copied using the rhs copier.
             * @param rhs Another tag value to be copied from.
             */
            tag_value(const tag_value &rhs);

            /**
             * Moves the copier, value and tag value from rhs into this instance.
             * Rhs will be left in an invalid_type state.
             * @param rhs Another tag value to be moved from.
             */
            tag_value(tag_value &&rhs);

            /**
             * Copy assigns the copier, value and tag value.
             * The value is copied using the rhs copier.
             * @param rhs Another tag value to be copy assigned from.
             */
            auto operator=(const tag_value &rhs) -> tag_value &;

            /**
             * Move assigns the copier, value and tag value.
             * The value is copied using the rhs copier.
             * @param rhs Another tag value to be move assigned from.
             */
            auto operator=(tag_value &&rhs) -> tag_value &;
             
            /**
             * Obtains the tag value.
             * @return Contained tag value type.
             */
            auto get_type() const -> tag_value_type;

            /**
             * Obtains a newly created value based on the provided template type
             * using the value stored in this instance.
             * @tparam Value type to obtain.
             * @return Contained value as type T.
             */
            template <class T>
            auto get_value() const -> T;

        private:
            tag_value_copier_t copier;
            std::unique_ptr<void, tag_value_deleter_t> value_ptr;
            tag_value_type type;
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
            ::rustfp::Result<std::string, std::string>;

        /**
         * Applies stored list of tag_rpms for replacement on the given string
         * content. Works on std::string type.
         * @param content str_type content which should contain the tags for
         * string replacements.
         */
        auto apply(const std::string &content) const ->
            ::rustfp::Result<std::string, std::string>;

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

        /**
         * Creates the correct copier function based on the template argument provided.
         */
        template <class T>
        auto make_copier() -> tag_value_copier_t
        {
            using udeleter_ptr_t = std::unique_ptr<void, tag_value_deleter_t>;

            return [](const udeleter_ptr_t &rhs) {
                return udeleter_ptr_t(
                    new T(*static_cast<const T *>(rhs.get())), rhs.get_deleter());
            };
        }

        /**
         * Creates the correct deleter function based on the template argument provided.
         */
        template <class T>
        auto make_deleter() -> tag_value_deleter_t {
            return [](void * const ptr) { delete static_cast<T *>(ptr); };
        }

        /**
         * Meant for compile time enum value inference for tag value. The base implementation
         * cannot be used and only specialized forms below are supported.
         */
        template <class invalid_type>
        struct tag_value_impl {
            static_assert(sizeof(invalid_type) < 0,
                "Usage of invalid_type type for tag_value_impl.");

            static constexpr tag_value_type type = tag_value_type::invalid_type;
        };

        template <>
        struct tag_value_impl<bool> {
            static constexpr tag_value_type type = tag_value_type::bool_type;
        };

        template <>
        struct tag_value_impl<char> {
            static constexpr tag_value_type type = tag_value_type::char_type;
        };

        template <>
        struct tag_value_impl<signed char> {
            static constexpr tag_value_type type = tag_value_type::char_type;
        };
        
        template <>
        struct tag_value_impl<short> {
            static constexpr tag_value_type type = tag_value_type::i16_type;
        };
        
        template <>
        struct tag_value_impl<int> {
            static constexpr tag_value_type type = tag_value_type::i32_type;
        };
        
        template <>
        struct tag_value_impl<long> {
            static constexpr tag_value_type type = tag_value_type::i32_type;
        };
        
        template <>
        struct tag_value_impl<long long> {
            static constexpr tag_value_type type = tag_value_type::i64_type;
        };
        
        template <>
        struct tag_value_impl<unsigned char> {
            static constexpr tag_value_type type = tag_value_type::u8_type;
        };
        
        template <>
        struct tag_value_impl<unsigned short> {
            static constexpr tag_value_type type = tag_value_type::u16_type;
        };

        template <>
        struct tag_value_impl<unsigned long> {
            static constexpr tag_value_type type = tag_value_type::u32_type;
        };
        
        template <>
        struct tag_value_impl<unsigned int> {
            static constexpr tag_value_type type = tag_value_type::u32_type;
        };
        
        template <>
        struct tag_value_impl<unsigned long long> {
            static constexpr tag_value_type type = tag_value_type::u64_type;
        };
        
        template <>
        struct tag_value_impl<float> {
            static constexpr tag_value_type type = tag_value_type::f32_type;
        };
        
        template <>
        struct tag_value_impl<double> {
            static constexpr tag_value_type type = tag_value_type::f64_type;
        };
        
        template <>
        struct tag_value_impl<long double> {
            static constexpr tag_value_type type = tag_value_type::long_double_type;
        };

        template <>
        struct tag_value_impl<std::string> {
            static constexpr tag_value_type type = tag_value_type::str_type;
        };
        
        template <class T>
        struct tag_value_impl<T *> {
            static constexpr tag_value_type type = tag_value_type::ptr_type;
        };

        template <class T>
        tag_value::tag_value(T &&value) :
            copier(make_copier<std::decay_t<T>>()),

            value_ptr(
                new std::decay_t<T>(std::forward<T>(value)),
                make_deleter<std::decay_t<T>>()),

            type(tag_value_impl<std::decay_t<T>>::type) {
            
        }

        inline tag_value::tag_value(const char value[]) :
            copier(make_copier<std::string>()),
            value_ptr(
                static_cast<void *>(new std::string(value)),
                details::make_deleter<std::string>()),
            type(tag_value_impl<std::string>::type) {
            
        }

        inline tag_value::tag_value(const tag_value &rhs) :
            copier(rhs.copier),
            value_ptr(copier(rhs.value_ptr)),
            type(rhs.type) {

        }

        inline tag_value::tag_value(tag_value &&rhs) :
            copier(move(rhs.copier)),
            value_ptr(move(rhs.value_ptr)),
            type(rhs.type) {

            rhs.type = tag_value_type::invalid_type;
        }

        inline auto tag_value::operator=(const tag_value &rhs) -> tag_value & {
            copier = rhs.copier;
            value_ptr = copier(rhs.value_ptr);
            type = rhs.type;
            return *this;
        }
        
        inline auto tag_value::operator=(tag_value &&rhs) -> tag_value & {
            copier = move(rhs.copier);
            value_ptr = move(rhs.value_ptr);

            type = rhs.type;
            rhs.type = tag_value_type::invalid_type;
            return *this;
        }

        inline auto tag_value::get_type() const -> tag_value_type {
            return type;
        }

        template <class T>
        auto tag_value::get_value() const -> T {
            return *reinterpret_cast<const T *>(value_ptr.get());
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
            std::vector<char> &machine_res) -> ::rustfp::Result<tag_state, std::string>
        {
            // fmt
            using fmt::format;

            // rustfp
            using ::rustfp::Err;
            using ::rustfp::Ok;
            using ::rustfp::Result;
            using ::rustfp::Unit;
            using ::rustfp::unit_t;

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
                    return Err(format("No matching tag value for tag name '{}'!", tag_name));
                }

                return Ok(cref(tap_rpms_itr->second));
            };

            const auto insert_fmt_value =
                [&find_tag_value](const vector<char> &imbued_tag_and_fmt, vector<char> &result) ->
                    Result<unit_t, string> {

                static constexpr char TARGET_CHARS[] = ":}";

                const auto imbued_tag_split_itr = find_first_of(
                    imbued_tag_and_fmt.cbegin(), imbued_tag_and_fmt.cend(),
                    cbegin(TARGET_CHARS), cend(TARGET_CHARS));

                const string tag_name(imbued_tag_and_fmt.cbegin() + 1, imbued_tag_split_itr);
                auto tag_res = find_tag_value(tag_name);
                RUSTFP_LET_REF(tag, tag_res);

                const auto imbued_fmt = "{" + string(
                    imbued_tag_split_itr, imbued_tag_and_fmt.cend());

                string fmt_value;

                switch (tag.get_type()) {
                case tag_value_type::bool_type:
                    fmt_value = format(imbued_fmt, tag.template get_value<bool>());
                    break;

                case tag_value_type::char_type:
                    fmt_value = format(imbued_fmt, tag.template get_value<int8_t>());
                    break;

                case tag_value_type::i16_type:
                    fmt_value = format(imbued_fmt, tag.template get_value<int16_t>());
                    break;

                case tag_value_type::i32_type:
                    fmt_value = format(imbued_fmt, tag.template get_value<int32_t>());
                    break;

                case tag_value_type::i64_type:
                    fmt_value = format(imbued_fmt, tag.template get_value<int64_t>());
                    break;

                case tag_value_type::u8_type:
                    fmt_value = format(imbued_fmt, tag.template get_value<uint8_t>());
                    break;

                case tag_value_type::u16_type:
                    fmt_value = format(imbued_fmt, tag.template get_value<uint16_t>());
                    break;

                case tag_value_type::u32_type:
                    fmt_value = format(imbued_fmt, tag.template get_value<uint32_t>());
                    break;

                case tag_value_type::u64_type:
                    fmt_value = format(imbued_fmt, tag.template get_value<uint64_t>());
                    break;

                case tag_value_type::f32_type:
                    fmt_value = format(imbued_fmt, tag.template get_value<float>());
                    break;

                case tag_value_type::f64_type:
                    fmt_value = format(imbued_fmt, tag.template get_value<double>());
                    break;

                case tag_value_type::long_double_type:
                    fmt_value = format(imbued_fmt, tag.template get_value<long double>());
                    break;

                case tag_value_type::str_type:
                    fmt_value = format(imbued_fmt, tag.template get_value<std::string>());
                    break;

                case tag_value_type::ptr_type:
                    fmt_value = format(imbued_fmt, tag.template get_value<void *>());
                    break;
                    
                default:
                    return Err(string("Attempting to use an already moved tag value!"));
                }

                result.insert(result.cend(), fmt_value.cbegin(), fmt_value.cend());
                return Ok(Unit);
            };

            const auto transfer_buf_to_res =
                [](std::vector<char> &buffer, std::vector<char> &result) {
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
                    // two open curly bracket detected, revert back to normal state
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
                    auto insert_res = insert_fmt_value(machine_buf, machine_res);
                    RUSTFP_RET_IF_ERR(insert_res);

                    machine_buf.clear();
                    machine_buf.push_back(fmt_char);

                    break;
                }

                case '\0': {
                    next_state = tag_state::end;

                    // insert tag value
                    auto insert_res = insert_fmt_value(machine_buf, machine_res);
                    RUSTFP_RET_IF_ERR(insert_res);
                    machine_buf.clear();

                    break;
                }

                default: {
                    next_state = tag_state::normal_text;

                    // insert tag value
                    auto insert_res = insert_fmt_value(machine_buf, machine_res);
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
        ::rustfp::Result<std::string, std::string> {

        // std
        using std::exception;
        using std::move;
        using std::string;
        using std::vector;

        // rustfp
        using ::rustfp::Err;
        using ::rustfp::Ok;
        using ::rustfp::Result;
        using ::rustfp::Unit;
        using ::rustfp::unit_t;

        try {
            auto state = details::tag_state::normal_text;
            vector<char> machine_buf;
            vector<char> machine_res;

            const auto manage_tag_state_machine =
                [this, &machine_buf, &machine_res, &state](const char fmt_char) ->
                    Result<unit_t, string> {

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
        ::rustfp::Result<std::string, std::string> {

        return apply(content.c_str());
    }

    template <class T>
    auto formatter::set_mapping(const std::string &tag, T &&value) & -> formatter & {
        auto tag_rpm_itr = tag_rpms.find(tag);

        if (tag_rpm_itr == tag_rpms.cend()) {
            // empty, so emplace new value
            tag_rpms.emplace(tag, details::tag_value(std::move(value)));
        } else {
            // key exists, so replace the value
            tag_rpm_itr->second = details::tag_value(std::move(value));
        }

        return *this;
    }

    template <class T>
    auto formatter::set_mapping(const std::string &tag, T &&value) && -> formatter {
        // calls the lvalue version
        return std::move(this->set_mapping(tag, std::move(value)));
    }

    inline auto make_formatter() -> formatter {
        return formatter{};
    }
}
