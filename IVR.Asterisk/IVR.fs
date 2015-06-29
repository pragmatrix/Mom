namespace IVR.Asterisk

open AsterNET.ARI.Actions

module Channels = 
    
    let answer id (channels : IChannelsActions) = 
        channels.Answer(id)

    let hangup id (channels: IChannelsActions) = 
        channels.Hangup(id)
