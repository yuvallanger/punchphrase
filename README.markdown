# Punchphrase - a passphrase input utility

A normal password of about 8 characters long can be deleted and re-typed quite easily, but that is not the case with passphrases where the length can easily reach a length 30 characters. In punchphrase will be experimenting with different passphrase input schemes trying to solve this problem. Punchphrase is an experimental software. It is not intended for any real world applications.

Let's talk about a few possible implementations.

A single line where each word is separated. The length of each word is visible, so it might have security issues.

    **** *** ***** ** ****** **** *****

Same as above, but the length of each word is not visible.

    **** **** **** **** **** **** ****

A line for each word, each length is visible.

    ****
    ***
    *****
    **
    ******
    ****
    *****

Same as above, but lengths are not displayed.

    ****
    ****
    ****
    ****
    ****
    ****
    ****

I will implement each of those.
