from django.contrib.auth import logout, login, authenticate
from django.shortcuts import render, redirect
from django.urls import reverse
from cryptography.fernet import Fernet
from accounts.forms import (
    RegistrationForm,
)
from .forms import UserLoginForm
from django.contrib.auth.models import User
from SecureApp.models import Post, userFriends


def login_view(request):
    next = request.GET.get('next')
    form = UserLoginForm(request.POST or None)
    if form.is_valid():
        username = form.cleaned_data.get('username')
        password = form.cleaned_data.get('password')
        user = authenticate(username=username, password=password)
        login(request, user)
        if next:
            return redirect(next)
        return redirect('home')
    context = {
        'form': form,
    }
    return render(request, "clogin.html", context)

def register(request):
    if request.method =='POST':
        form = RegistrationForm(request.POST)
        if form.is_valid():
            form.save()
            return redirect(reverse('home'))
    else:
        form = RegistrationForm()
    args = {'form': form}
    return render(request, 'reg_form.html', args)


def view_profile(request, pk=None):
    k = Fernet.generate_key()
    crypter = Fernet(k)
    if pk:
        user = User.objects.get(pk=pk)
        friend = userFriends.objects.get(current_user=user)
        friends = friend.users.all()
        isFriend = friends.filter(id=request.user.id).exists()
        userPosts = Post.objects.filter(user_id=user.id)
        if(not isFriend):
            for i in userPosts:
                encrypted = crypter.encrypt(i.post.encode())
                i.post = encrypted
    else:
        user = request.user
        userPosts = Post.objects.filter(user_id=user.id)
        isFriend = True
    args = {'user': user,'posts':userPosts,"friend":isFriend}
    return render(request, 'profile.html', args)

def logout_view(request):
    logout(request)
    return redirect('/')