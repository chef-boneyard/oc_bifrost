-record(user_context, {user}).

-record(auth_req, {caller,
                   check_type,
                   perm,
                   entry_type,
                   entries}).
