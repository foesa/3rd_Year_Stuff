from django.contrib.auth import logout, login, authenticate
from django.shortcuts import render, redirect
from django.urls import reverse
from accounts.forms import (
    RegistrationForm,
)
from .forms import UserLoginForm
from django.contrib.auth.models import User

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
    if pk:
        user = User.objects.get(pk=pk)
    else:
        user = request.user
    args = {'user': user}
    return render(request, 'profile.html', args)

def logout_view(request):
    logout(request)
    return redirect('/')