function force(thunk)
{
    return thunk();
};

function constant_arg_for_dvl_stream(argument, dvl_stream)
{
    function do_it(dvl_stream)
    {
        return [dvl_stream[0],
                function () { return do_it(dvl_stream[1](argument)) }];
    };
    return do_it(dvl_stream);
};

function stream_for_each(f, stream)
{
    return [f(stream[0]),
            function () { return stream_for_each(f, force(stream[1])) }];
};

function stream_take(count, stream)
{
    if (count == 0)
        return stream;
    else
        return stream_take(count-1, force(stream[1]));
};

function drive(count, step, dt)
{
    return stream_take(count,
                       constant_arg_for_dvl_stream(dt, (__main__())(step)));
};
