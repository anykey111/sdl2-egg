#include <SDL2/SDL_timer.h>
#include <SDL2/SDL_events.h>
#include <stdbool.h>

#define SDL2TIMER_CALLBACK 0xCDE0

static Uint32 callback_wrapper(Uint32 interval, void *repeat)
{
    SDL_Event event;
    SDL_UserEvent user_event;
    user_event.type = SDL_USEREVENT;
    user_event.code = SDL2TIMER_CALLBACK;
    user_event.data1 = 0;
    user_event.data2 = 0;
    event.type = SDL_USEREVENT;
    event.user = user_event;
    SDL_PushEvent(&event);
    return repeat ? interval : 0;
}

static SDL_TimerID sdl2_add_timer(Uint32 interval, bool repeat)
{
    return SDL_AddTimer(interval, callback_wrapper, (void *)repeat);
}
